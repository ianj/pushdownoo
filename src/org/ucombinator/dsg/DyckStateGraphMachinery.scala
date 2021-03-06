package org.ucombinator.dsg
import org.ucombinator.dalvik.cfa.gc.GarbageCollectorTrait
import org.ucombinator.utils.FancyOutput
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.utils.Debug
import org.ucombinator.utils.{StringUtils, AIOptions, FancyOutput}
import tools.nsc.io.Directory


trait DyckStateGraphMachinery extends StateSpace{ 
  self: GarbageCollectorTrait with FancyOutput =>

  /**
   * Abstract componnents
   */

 // type Frame
//  type ControlState

  type EntryExp // term
  type AbsValue
//  type Addr

 // type SharedStore = Map[Addr, Set[AbsValue]]
 type SharedStore = Addr :-> Set[Value]
 // def initState(e: Stmt, methP: String): (ControlState, Kont)

  def step(q: ControlState, k: Kont, frames: Kont, store: SharedStore): Set[(StackAction[Frame], ControlState, SharedStore)]

  // the following four, not sure about the real uage
  def mustHaveOnlyEmptyContinuation(s: ControlState): Boolean

  def canHaveEmptyContinuation(s: ControlState): Boolean

  def canHaveSwitchFrames: Boolean
  
  def isStoreSensitive(s: ControlState): Boolean

  /**
   *  Dyck State graph Nodes
   */
  type S = ControlState
  type Nodes = Set[S]
  type Kont = List[Frame]

  /**
   * Dyck State graph Edges
   */
  sealed case class Edge(source: S, g: StackAction[Frame], target: S)

  type Edges = Set[Edge]

  /**
   * DSG = (S,E,s0) -- a Dyck State graph
   * implicitly parametrized with ? (a set of frames)
   * s0 \in S -- initial node
   */
  sealed case class DSG(nodes: Set[S], edges: Edges, s0: S)

  /**
   * Compute the leas-fixed point by Kleene iteration 
   */
  
  def evaluateDSG(e: Stmt, methP: String) : (DSG, SharedStore) ={
    val initial = initState(e, methP)
    val initS = initial._1
    
    //compute the LFP recursively: trap
    def eval(next: DSG, helper: NewDSGHelper, shouldProceed: Boolean, statesToVisit: Set[S], store: SharedStore):
    (DSG, NewDSGHelper, SharedStore) = {
     // System.out.println("\n The DSG graph: ")
       // dumpDSGGraph2(next)
      if (!shouldProceed) {
        (next, helper,store)
      }else if (interrupt && next.edges.size > interruptAfter){
         (next, helper, store) //ce
      } else {
        val (next2, helper2, goAgain, newToVisit, newStore) 
        = iterateDSG(next,helper, statesToVisit, store)
        eval(next2, helper2, goAgain, newToVisit, newStore)
      }
    }
     val firstDSG = DSG(Set(initS), Set(), initS)
    val firstHelper = new NewDSGHelper
    val (nextDSG, nextHelper, hasNew, toVisit, firstStore) = iterateDSG(firstDSG, firstHelper, Set(initS), Map.empty)
    


    val (resultDSG, _, newStore) = eval(nextDSG, nextHelper, hasNew, toVisit, firstStore)

    (resultDSG, newStore)
  }
  
   /**
   * Monotonic DSG iteration function
   * denoted as 'f' in the paper
   */
  private def iterateDSG(dsg: DSG, helper: NewDSGHelper, toVisit: Set[S], store: SharedStore): (DSG, NewDSGHelper, Boolean, Set[S], SharedStore) = dsg match {
    case DSG(ss, ee, s0) => {

      val newNodesEdgesStores: Set[(S, Edge, SharedStore)] = for {
        s <- toVisit
        
        kont <- helper.getRequiredKont(s, s0)
        possibleFrames = helper.getPossibleStackFrames(s)
        (g, s1, littleStore) <- step(s, kont, possibleFrames, store)
      } yield (s1, Edge(s, g, s1), littleStore)

      val (obtainedStates, obtainedEdges, obtainedStores) = newNodesEdgesStores.unzip3

      // Transform switch edges to pairs of push/pop edges
      val noSwitchesEdges: Edges = if (canHaveSwitchFrames) processSwitchEdges(obtainedEdges) else obtainedEdges
      // Collect new states after decoupling switches
      val newStates: Nodes = (if (canHaveSwitchFrames) {
        val nodes: Set[S] = (noSwitchesEdges -- ee).map {
          case Edge(source, _, target) => target
        }
        nodes ++ obtainedStates
      } else obtainedStates)

      val newEdges = noSwitchesEdges -- ee

      helper.update(newEdges)
     
      val newStore: SharedStore = obtainedStores.foldLeft(store)(_ ++ _)
      
      // global widening
      val wideneningStore : SharedStore = getMonovariantStore(filterRegisterStates(ss))
      val joinedNewStore = mergeStores(newStore, List(wideneningStore))
     
      val newSEpsNext = newStates.flatMap(s => helper.getEpsNextStates(s))
      //println("NEW elspang "+ newSEpsNext.toList.length)
      val storeSS = getStoreSensitiveStates(ss)
  
      val newToVisit = (newStates	
        // Lemma 1 (newEps)
        ++ newSEpsNext)
        // Lemma 2 (store-sensitive)
       // ++ storeSS) ////// NOTE: the store sensitive states are from the dyck state graph nodes. which has been explored before.

      // S' = ...
      val ss1: Nodes = ss ++ newStates + s0

      // E' = ...
      val ee1 = (ee ++ newEdges)
      
      val cond1 = !newEdges.subsetOf(ee)
    
      val cond2 = (store != newStore)

      val shouldProceed = cond1 || cond2

      println(progressPrefix + " " + ss1.size + "  " + ee1.size + " " + newSEpsNext.toList.length +" \n")
     
      (DSG(ss1, ee1, s0), helper, shouldProceed, newToVisit, joinedNewStore )//newStore)
    }
  }
      
      
    
    
  
  sealed class NewDSGHelper {

    import scala.collection.mutable.{Map => MMap, HashMap => MHashMap}

    private val epsPreds: MMap[S, Nodes] = new MHashMap
    private val epsSuccs: MMap[S, Nodes] = new MHashMap
    private val topFrames: MMap[S, Set[Frame]] = new MHashMap

    /**
     * Let s1 --[+f]--> s2_1 --> .... --> s2_n --[-f]--> s3
     * Then predForPushFrame((s2_i, f)) contains s1
     */
    private val predForPushFrame: MMap[(S, Frame), Nodes] = new MHashMap
    private val nonEpsPreds: MMap[S, Nodes] = new MHashMap

    ////////////////// Public methods //////////////////

    def update(newEdges: Set[Edge]) {
      for (e <- newEdges) {
        e match {
          case Edge(s1, Eps, s2) => equalize(s1, s2)
          case Edge(s1, Pop(f), s2) => processPop(s1, f, s2)
          case Edge(s1, Push(f), s2) => processPush(s1, f, s2)
          case Edge(_, se@Switch(_, _, _), _) => throw new DSGException("Illegal switch edge: " + se)
        }
      }
    }
    

    

    /**
     * 
     * Constructs a fake continuation with only a top frame (if any)
     */
    def getRequiredKont(s: S, s0: S): Set[Kont] = {
      val frames = gets(topFrames, s)
      if (frames.isEmpty) {
        Set(List())
      } else {
        // the current control state is finalState,
        // which will return true
        if (mustHaveOnlyEmptyContinuation(s)) {
          Set(List())

          /**
           * (REMARK)
           * [Valid final candidate]
           * Should carry a value and be epsilon-reachable
           * from the initial state
           */
        } 
        else if (canHaveEmptyContinuation(s)
          && (getEpsPredStates(s)).contains(s0)) {
          frames.map(f => List(f)) + List()
        } 
        else {
          frames.map(f => List(f))
        }
      }
    }

    /**
     * Necessary for abstract GC
     *
     * (REMARK)
     * [Dyck property exploited]
     * Compute recursively all possible frames that can be
     * somewhere in the stack for a state 's'
     */
    def getPossibleStackFrames(s: S): Kont = {
      if (!shouldGC) {
        // We don't deed it if there is no --gc flag
        return Nil
      }

      // initial -- just top frames
      var workSet: Nodes = Set(s) ++ getEpsPredStates(s)

      // first iteration
      var frames = workSet.flatMap(s => gets(topFrames, s))

      // get non-eps preds
      val neps = workSet.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
      val toProcess = neps ++ neps.flatMap(getEpsPredStates(_))
      var newWorkSet: Nodes = workSet ++ toProcess

      def iterate(delta: Nodes) {
        if (!workSet.equals(newWorkSet)) {
          // compute new frames
          frames = frames ++ delta.flatMap(s => gets(topFrames, s))
          // update old working set
          workSet = newWorkSet
          // compute new states
          val neps1 = delta.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
          val delta1 = neps1 ++ neps1.flatMap(getEpsPredStates(_))
          newWorkSet = workSet ++ delta1

          iterate(delta1)
        }
      }

      iterate(toProcess)
      frames.toList
    }

    def getEpsNextStates(s: S): Nodes = gets(epsSuccs, s)

    ///////////////// Inner methods ////////////////////

    private def getEpsPredStates(s: S): Nodes = gets(epsPreds, s)


    /**
     * "Equalize" eps-predecessors & eps-successors
     * when an eps-transition s1 --[eps]--> s2 is added
     */
    private def equalize(s1: S, s2: S) {
      val preds = Set(s1) ++ gets(epsPreds, s1)
      val nexts = Set(s2) ++ gets(epsSuccs, s2)

      // Add new successors
      for (s <- preds) {
        puts(epsSuccs, s, nexts)
      }

      // Add new predecessors and top frames
      val topFramesToAdd = preds.flatMap(x => gets(topFrames, x))
      for (s <- nexts) {
        puts(epsPreds, s, preds)
        puts(topFrames, s, topFramesToAdd)
        for (f <- gets(topFrames, s1)) {
          val predForPushForS1 = gets(predForPushFrame, (s1, f))
          puts(predForPushFrame, (s, f), predForPushForS1)
        }
      }
    }

    /**
     * Update topFrames and predForPushFrames for a new edge s1 --[+f]--> s2
     */
    private def processPush(s1: S, f: Frame, s2: S) {
      val nexts = Set(s2) ++ gets(epsSuccs, s2)
      for (s <- nexts) {
        puts(topFrames, s, Set(f))
        puts(predForPushFrame, (s, f), Set(s1))
        puts(nonEpsPreds, s, Set(s1))
      }
    }

    /**
     * Update eps-graphs for a new egde s1 --[-f]--> s2
     */
    private def processPop(s1: S, f: Frame, s2: S) {
      val newEpsPreds = gets(predForPushFrame, (s1, f))
      for (s <- newEpsPreds) {
        equalize(s, s2)
      }
    }

    /**
     * Utility function for multimaps
     */
    private def puts[A, B](map: MMap[A, Set[B]], key: A, newVals: Set[B]) {
      val oldVals = map.getOrElse(key, Set())
      val values = oldVals ++ newVals
      map += ((key, values))
    }

    private def gets[A, B](map: MMap[A, Set[B]], key: A): Set[B] = map.getOrElse(key, Set())

  }
   private def getStoreSensitiveStates(ss: Set[S]) = ss.filter(isStoreSensitive(_))
   
   /**************************************************************
   * Some utility methods
   ***************************************************************/

  /**
   * The function exploits the balanced structure of paths in DSG
   * So any "new" stack action cannon affect the status of successor nodes,
   * only "close" predecessors might become epsilon-predecessors.
   */
  def stackActionsEquivalent(g1: Frame, g: Frame): Boolean = {
    g1 == g
  }

  private def processSwitchEdges(edges: Edges): Edges = edges.flatMap {
    case Edge(source, Switch(popped, target: S, pushed), mid) => Set(
      Edge(source, Pop(popped), mid),
      Edge(mid, Push(pushed), target)
    )
    case e => Set(e)
  }
  
  /**
   * Prints DSG according to the passed parameters
   */
  

  def prettyPrintDSG2(dsg: DSG): String = {

    val edges = dsg.edges
    
    val states: Set[ControlState] = dsg.nodes.asInstanceOf[Set[ControlState]]

    var stateCounter = 0
    val map: Map[ControlState, Int] = states.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap

    val buffer = new StringBuffer
    buffer.append("digraph BST {\nsize=\"6,4\" \n ")

    var list: List[String] = List()
    for (Edge(s, g, s1) <- edges if s != s1) {
      val buf = new StringBuffer()
      buf.append("\"" + prettyPrintState2(s, map) + "\"")
      buf.append(" -> ")
      buf.append("\"" + prettyPrintState2(s1, map) + "\"")

      if (!simplify) {
        buf.append(" [label=\"")
        buf.append(StringUtils.truncateIfLong(g.toString, 100))
        buf.append("\"]")
      }

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }
  
  import org.ucombinator.utils.StringUtils._

  def dumpDSGGraph2( resultDSG: DSG): String = {

    import java.io._

    val graphs = new Directory(new File(graphsDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(".")
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }


    val path = subfolderPath + File.separator + (new java.util.Date()).getTime + ".gv"
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
    writer.write(prettyPrintDSG2(resultDSG))
    writer.close()
    path
  }


}
class DSGException(s: String) extends Exception(s)
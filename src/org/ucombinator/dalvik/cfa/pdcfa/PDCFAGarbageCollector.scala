package org.ucombinator.dalvik.cfa.pdcfa

import org.ucombinator.dalvik.cfa.gc.DalvikGarbageCollector
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual
import org.ucombinator.dalvik.syntax.StForEqual
import org.ucombinator.dalvik.syntax.FieldAssignStmt
import org.ucombinator.dalvik.syntax.NonStaticFieldExp
import org.ucombinator.dalvik.syntax.AssignAExpStmt
import org.ucombinator.dalvik.syntax.AtomicOpExp


trait PDCFAGarbageCollector extends DalvikGarbageCollector with StackCESKMachinary with StmtForEqual {
  
   def getRootAddrs(c: ControlState, frames: List[Frame]): Set[Addr] = {
     
    val  addrsOfStateFP = c match {
       case ErrorState(_,_) | FinalState() => Set.empty
       case ps@PartialState(StForEqual(stmt, nxss, lss, clsP, methP), curFP, store, kptr, t) => {
         //getAddrsofCurFP(curFP, store)
         //turn to the following!!
         val curFPAddrs = getAddrsofCurFP(curFP, store)
         
        
         if(doLRA) {
           
            filterLiveRegAddrs(ps.st, curFPAddrs)}
         else curFPAddrs
       }
     }
 
    val stackAddrs : Set[Addr] = c match {
       case ErrorState(_,_) | FinalState() => Set.empty
       case PartialState(stmt, curFP, store, kptr, t) => {
          frames.foldLeft(Set[Addr]())((res: Set[Addr], f: Frame) => {
            val sf = getRootAddrsFromStack(f,store)
          
            res ++ sf
            })
          }
     }
     addrsOfStateFP ++ stackAddrs
   }
   
    def filterLiveRegAddrs(st: StForEqual, addrs: Set[Addr]) : Set[Addr] = {
   //   val stStr = CommonUtils.constrDistinctStatementStr(st)
      val liveRegs = Stmt.liveMap(st )
      
      addrs.filter{
         case RegAddr(ifp, offs) => { liveRegs.contains(offs) }
      }
   }
}
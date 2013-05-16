package org.ucombinator.dalvik.cfa.cesk


/*
 *  more statistics needed. 
 */

case class CFAStatistics 
(timeSec: Long,
 //numExp: Int,
 //numVars: Int, this one might not be available
 //numSingletons: Int,
 numStates: Int,
 numEdges: Int,
 interrupted: Boolean)
package org.encalmo.fea

/** Final element load's result */
case class ElementResult[A <: FiniteElement] (
        /** Referenced element */
        element:A,
        /** Corner nodes results */
        nodeResults:Seq[NodeResult] = Seq(),
        /** Gauss points stresses */
        gaussPointsStresses:Seq[StressAtPoint] = Seq()
){
    
}
package org.encalmo.fea

/** Final element load's result */
case class ElementResult[A <: FiniteElement] (
        /** Referenced element */
        element:A,
        /** cornerStresses */
        gaussPointsStresses:Seq[Seq[Option[Double]]] = Seq()
){
    
}
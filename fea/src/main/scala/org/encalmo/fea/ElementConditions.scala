package org.encalmo.fea

/** Element's material, thickness and surface load */
case class ElementConditions[A <: FiniteElement] (
        /** Referenced element */
        element:A,
        /** Material properties */
        material:Material,
        /** Thickness, if applicable */
        thickness:Double = 0,
        /** Optional surface load and forces, depends on element type */
        load:OptDoubleSeq = None
) {
    
    lazy val matinfo:Seq[Any] = element.createMatinfo(material,thickness,load)

}
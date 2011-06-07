package org.encalmo.fea.z88

/**
 * Finite Element trait
 */
trait FiniteElement {
    
    /** Finite element type atrtibutes */
    val attr:FiniteElementAttr
    
    /** Element number */
    var no:Option[Long] = None
    /** Element's nodes */
    def nodes:Seq[Node]
    /** Material */
    def material:Material

}

trait FiniteElementAttr {
    
    /** Dimension of the structure (2 or 3) */
    def dimension:Int
    /** Number of degrees of freedom */
    def dof:Int
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG:Int
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG:Int
    
}
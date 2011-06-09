package org.encalmo.fea.z88

/**
 * Finite Element trait
 */
trait FiniteElement extends Ordered[FiniteElement] with Numbered {
    
    /** Finite element type atrtibutes */
    val attr:FiniteElementAttr
    
    /** Element's number */
    var no:Int = -1
    def no_(i:Int) = {if(no<0 && i>0) no = i}
    /** Element's nodes */
    def nodes:Seq[Node]
    /** Material */
    def material:Material
    
    override def compare(elem:FiniteElement) = {
        if(no>=0 && elem.no>=0){
            no compare elem.no
        } else {
            (nodes,elem.nodes) match {
                case (Seq(),Seq()) => 0
                case (Seq(), _ ) => -1
                case _ => nodes.first compare elem.nodes.first 
            }
        }
    }

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
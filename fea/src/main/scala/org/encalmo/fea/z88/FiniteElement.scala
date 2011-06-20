package org.encalmo.fea.z88

import org.encalmo.fea.Vector

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
    /** Full material info, depends on element */
    def matinfo:Seq[Any]
    /** Compares two finite elements */
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
    /** Center of gravity */
    def center:Vector
    /** Verbal node's position descriptions */
    def nodesPositionDescription:String = nodes.foldLeft[String]("")((l,r) => l+" "+r.positionDescription)
    /** Verbal node's position symbols */
    def nodesPositionSymbol:String = nodes.foldLeft[String]("")((l,r) => l+" "+r.positionSymbol)

}

trait FiniteElementAttr {
    
    /** Type of element */
    def elemtype:Int
    /** Dimension of the structure (2 or 3) */
    def dimension:Int
    /** Number of degrees of freedom */
    def dof:Int
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG:Int
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG:Int
    /** Integration order */
    def intorder:Int
    
}
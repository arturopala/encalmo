package org.encalmo.fea

/**
 * Finite Element trait
 */
trait FiniteElement extends Ordered[FiniteElement] with Numbered {
    
    /** Finite element type atrtibutes */
    val attr:FiniteElementType
    /** Element's number */
    var no:Int = -1
    def no_(i:Int) = {if(no<0 && i>0) no = i}
    /** Element's nodes */
    def nodes:IndexedSeq[Node]
    /** Element's node numbers */
    lazy val nodesnumbers:Seq[Int] = nodes.map(_.no)
    /** Compares two finite elements */
    override def compare(elem:FiniteElement) = {
        if(no>=0 && elem.no>=0){
            no compare elem.no
        } else {
            (nodes,elem.nodes) match {
                case (Seq(),Seq()) => 0
                case (Seq(), _ ) => -1
                case _ => nodes.head compare elem.nodes.head 
            }
        }
    }
    /** Center of gravity */
    def center:Vector
    /** Verbal node's position descriptions */
    def nodesPositionDescription:String = nodes.foldLeft[String]("")((l,r) => l+" "+r.positionDescription)
    /** Verbal node's position symbols */
    def nodesPositionSymbol:String = nodes.foldLeft[String]("")((l,r) => l+" "+r.positionSymbol)
    /** abstract: Creates matinfo data sequence */
    def createMatinfo(material:Material,thickness:Double,load:OptDoubleSeq):Seq[Any]
    /** Finds node at given position */
    def findNode(x:Double = 0, y:Double = 0, z:Double = 0):Option[Node] = nodes.find(n => n.isAt(x,y,z))
    /** Finds node at given position */
    def findNode(v:Vector):Option[Node] = nodes.find(n => n.isAt(v))

}

trait FiniteElementType{
    
    /** Type of element */
    def elemtype:Int
    /** Name of type*/
    def name:String
    /** Dimension of the structure (2 or 3) */
    def dimension:Int
    /** Number of degrees of freedom */
    def dof:Int
    /** Number of element's nodes */
    def nodes:Int
    /** Number of element's corners */
    def corners:Int
    /** Number of element's gausspoints */
    def gausspoints:Int
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG:Int
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG:Int
    /** Integration order */
    def intorder:Int
    
}
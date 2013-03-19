package org.encalmo.fea

/**
 * Finite element
 * PLATE NO.20 WITH 8 NODES
 */
case class Plate20(
        override val nodes:IndexedSeq[Node]
) extends FiniteElement {
    
    /** Finite element type atrtibutes */
    override val attr = Plate20Type
    /** Center of gravity */
    override lazy val center:Vector = nodes(0).coordinates middle nodes(3).coordinates
    
    def printout = {
        nodes.foreach(n => {n.printout; Console.println})
    }
    /** Creates matinfo data sequence */
    def createMatinfo(material:Material,thickness:Double,load:OptDoubleSeq):Seq[Any] = {
        Seq[Any](material.E,material.P,attr.intorder,thickness) :+ (load.map(_ match {case Seq(x) => x.getOrElse(0d); case _ => 0d}).getOrElse(0d))
    }

}

/**
 * Finite element type attributes
 * PLATE NO.20 WITH 8 NODES
 */
object Plate20Type extends FiniteElementType { 
    
    /** Type of element */
    override def elemtype:Int = 20
    /** Name of type*/
    def name:String = "PLATE NO.20 WITH 8 NODES"
    /** Dimension of the structure (2 or 3) */
    override def dimension:Int = 2
    /** Number of element's nodes */
    override def nodes:Int = 8
    /** Number of element's corners */
    override def corners:Int = 4
    /** Number of element's gausspoints */
    override def gausspoints:Int = 4
    /** Number of degrees of freedom */
    override def dof:Int = 3
    /** Beam flag IBFLAG (0 or 1) */
    override def IBFLAG:Int = 0
    /** Plate flag IPFLAG (0 or 1) */
    override def IPFLAG:Int = 1
    /** Integration order */
    override def intorder:Int = 2
    
}
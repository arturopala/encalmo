package org.encalmo.fea

/**
 * Finite element
 * Z88 PLATE NO.19 WITH 16 NODES
 */
case class Plate19(
        override val nodes:Seq[Node]
) extends FiniteElement {
    
    /** Finite element type atrtibutes */
    override val attr = Plate19Type
    /** Center of gravity */
    override lazy val center:Vector = nodes(0).c middle nodes(15).c
    
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
 * Z88 PLATE NO.19 WITH 16 NODES
 */
object Plate19Type extends FiniteElementType {

    /** Type of element */
    override def elemtype:Int = 19
    /** Dimension of the structure (2 or 3) */
    override def dimension:Int = 2
    /** Number of element's nodes */
    override def nodes:Int = 16
    /** Number of element's corners */
    override def corners:Int = 4
    /** Number of element's gausspoints */
    override def gausspoints:Int = 16
    /** Number of degrees of freedom */
    override def dof:Int = 3
    /** Beam flag IBFLAG (0 or 1) */
    override def IBFLAG:Int = 0
    /** Plate flag IPFLAG (0 or 1) */
    override def IPFLAG:Int = 1
    /** Integration order */
    override def intorder:Int = 4
    
}
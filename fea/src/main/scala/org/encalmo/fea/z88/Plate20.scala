package org.encalmo.fea.z88

import org.encalmo.fea.Vector

/**
 * Finite element type
 * PLATE NO.20 WITH 8 NODES
 */
case class Plate20(
        override val nodes:Seq[Node],
        override val material:Material = null,
        thickness:Double = 0,
        load:Option[Seq[Option[Double]]] = None
) extends FiniteElement {
    
    /** Finite element type atrtibutes */
    override val attr = Plate20Attr
    /** Applies material, thickness and surface load functions */
    def apply(materialFx:FiniteElement=>Material,
            thicknessFx:FiniteElement=>Double,
            surfaceLoadFx:FiniteElement=>Option[Seq[Option[Double]]]):Plate20 = {
        copy(material = materialFx(this),thickness = thicknessFx(this), load = surfaceLoadFx(this))
    }
    /** Full material info, depends on element */
    override lazy val matinfo:Seq[Any] = Seq(material.E,material.P,attr.intorder,thickness) :+ (takeLoad)
    /** Center of gravity */
    override lazy val center:Vector = nodes(0).c middle nodes(3).c
    
    def printout = {
        nodes.foreach(n => {n.printout; Console.println})
    }
    /** Helper function */
    private def takeLoad:Double = load.map(_ match {case Seq(x) => x.getOrElse(0d); case _ => 0d}).getOrElse(0d)
}

/**
 * Finite element type attributes
 * PLATE NO.20 WITH 8 NODES
 */
object Plate20Attr extends FiniteElementAttr {
    
    /** Type of element */
    override def elemtype:Int = 20
    /** Dimension of the structure (2 or 3) */
    override def dimension:Int = 2
    /** Number of degrees of freedom */
    override def dof:Int = 3
    /** Beam flag IBFLAG (0 or 1) */
    override def IBFLAG:Int = 0
    /** Plate flag IPFLAG (0 or 1) */
    override def IPFLAG:Int = 1
    /** Integration order */
    override def intorder:Int = 2
    
}
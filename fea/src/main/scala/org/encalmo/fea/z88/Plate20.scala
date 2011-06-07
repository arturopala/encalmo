package org.encalmo.fea.z88

/**
 * Finite element type
 * PLATE NO.20 WITH 8 NODES
 */
case class Plate20(
        override val nodes:Seq[Node],
        thickness:Double,
        override val material:Material
) extends FiniteElement {
    
    /** Finite element type atrtibutes */
    override val attr = Plate20Attr
    
    def printout = {
        nodes.foreach(n => {n.printout; Console.println})
    }

}

/**
 * Finite element type attributes
 * PLATE NO.20 WITH 8 NODES
 */
object Plate20Attr extends FiniteElementAttr {
    
    /** Dimension of the structure (2 or 3) */
    override def dimension:Int = 2
    /** Number of degrees of freedom */
    override def dof:Int = 3
    /** Beam flag IBFLAG (0 or 1) */
    override def IBFLAG:Int = 0
    /** Plate flag IPFLAG (0 or 1) */
    override def IPFLAG:Int = 1
    
}
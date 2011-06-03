package org.encalmo.fea.z88

/**
 * PLATE NO.20 WITH 8 NODES
 */
case class Plate20(
        nodes:Seq[Node],
        thickness:Double,
        material:Material
) extends FiniteElement {
    
    def printout = {
        nodes.foreach(n => {n.printout; Console.println})
    }

}
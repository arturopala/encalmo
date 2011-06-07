package org.encalmo.fea.z88

import org.encalmo.fea.Vector
import scala.annotation.tailrec

/**
 * Mesh node
 */
case class Node(
        /** Offset from base vector */
        d:Vector,
        /** Optional base node */
        base:Option[Node] = None,
        /** Node displacement vector */
        U:Option[Vector] = None,
        /** Nodal forces vector */
        F:Option[Vector] = None
){
    
    /** Node number */
    var no:Option[Long] = None
    /** Real node's coordinates */
    lazy val coordinates:Vector = if(!base.isDefined) {d} else {d+base.get.coordinates}
    /** Coordinates shorthand */
    def c = coordinates
    /** Set that node as base */
    def join(n:Node) = copy(base=Option(n))
    /** Print out node's coordinates */
    def printout = {
        val c = coordinates
        Console.print(c.x)
        Console.print(" ")
        Console.print(c.y)
        Console.print(" ")
        Console.print(c.z)
    }
    
}
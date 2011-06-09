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
        
) extends Ordered[Node] with Numbered {
    
    /** Simple constructor */
    def this(x:Double,y:Double,z:Double) = this(Vector(x,y,z))
    
    /** Node number */
    var no:Int = -1
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
    /** from {@link Ordered} */
    override def compare(node:Node):Int = {
        if(no>=0 && node.no>=0){
            no compare node.no
        } else {
            c.z.compare(node.c.z) match {
                case 0 => {
                    c.y.compare(node.c.y) match {
                        case 0 => c.x.compare(node.c.x)
                        case r => r
                    }
                }
                case r => r
            }
        }
    }
    
}

object Node {
    
    def apply(x:Double,y:Double,z:Double) = new Node(Vector(x,y,z))
    
}
package org.encalmo.fea.z88

import org.encalmo.fea.Vector
import scala.annotation.tailrec

case class Node(
        d:Vector,
        base:Option[Node] = None,
        U:Option[Vector] = None,
        F:Option[Vector] = None
){
    
    lazy val coordinates:Vector = if(!base.isDefined) {d} else {d+base.get.coordinates}
    
    def c = coordinates
    
    def join(n:Node) = copy(base=Option(n))
    
    def printout = {
        val c = coordinates
        Console.print(c.x)
        Console.print(" ")
        Console.print(c.y)
        Console.print(" ")
        Console.print(c.z)
    }
    
}
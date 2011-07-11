package org.encalmo.fea

import scala.annotation.tailrec

/**
 * Mesh node
 */
case class Node(
        
        /** Offset from base vector */
        d:Vector,
        /** Optional base node */
        base:Option[Node] = None,
        /** Node position: 0-inside, 1-surface, 2-edge, 3-corner  **/
        position:Int = 0
        
) extends Ordered[Node] with Numbered {
    
    /** Simple constructor */
    def this(x:Double,y:Double,z:Double) = this(Vector(x,y,z))
    /** Simple constructor with position */
    def this(x:Double,y:Double,z:Double,p:Int) = this(Vector(x,y,z),None,p)
    
    /** Node's number */
    var no:Int = -1
    def no_(i:Int) = {if(no<0 && i>0) no = i}
    /** Computed node's coordinates */
    lazy val coordinates:Vector = if(!base.isDefined) {d} else {d+base.get.coordinates}
    /** X coordinate */
    def x:Double = coordinates.x
    /** Y coordinate */
    def y:Double = coordinates.y
    /** Z coordinate */
    def z:Double = coordinates.z
    /** Coordinates shorthand */
    def c = coordinates
    /** Set that node as base */
    def join(n:Node) = copy(base = Option(n))
    /** Print out node's coordinates */
    def printout = {
        val c = coordinates
        Console.print(x)
        Console.print(" ")
        Console.print(y)
        Console.print(" ")
        Console.print(z)
    }
    /** from {@link Ordered} */
    override def compare(node:Node):Int = {
        if(no>=0 && node.no>=0){
            no compare node.no
        } else {
            z.compare(node.z) match {
                case 0 => {
                    y.compare(node.y) match {
                        case 0 => x.compare(node.x)
                        case r => r
                    }
                }
                case r => r
            }
        }
    }
    
    /** True if node is in the corner */
    def inCorner:Boolean = position==3
    /** True if node is on the surface */
    def onEdge:Boolean = position==2
    /** True if node is on the surface */
    def onEdgeOrInCorner:Boolean = position==2 || position==3
    /** True if node is on the edge */
    def onSurface:Boolean = position==1
    /** True if node is inside the solid */
    def isInside:Boolean = position==0
    /** True if node is on the edge */
    def onSurfaceOrInside:Boolean = position==1 || position==0
    /** Verbal position description */
    def positionDescription:String = position match {
        case 3 => "corner"
        case 2 => "edge"
        case 1 => "surface"
        case 0 => "inside"
    }
    /** Position symbol */
    def positionSymbol:String = position match {
        case 3 => "C"
        case 2 => "E"
        case 1 => "S"
        case 0 => "I"
    }
    /** String representation */
    override def toString:String = "Node #"+no+" "+coordinates+" "+positionDescription
    /** Test if node is at given position */
    def isAt(x:Double = 0, y:Double = 0, z:Double = 0) = coordinates.equals(x,y,z)
    /** Test if node is at given position */
    def isAt(v:Vector) = coordinates.equals(v)
    
}

/** Node factory */
object Node {
    
    def apply(x:Double,y:Double,z:Double):Node = new Node(Vector(x,y,z))
    def apply(x:Double,y:Double,z:Double,p:Int):Node = new Node(Vector(x,y,z),None,p)
    def apply(v:Vector):Node = new Node(v)
    def apply(v:Vector,p:Int):Node  = new Node(v,None,p)
    
}
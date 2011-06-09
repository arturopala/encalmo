package org.encalmo.fea

/**
 * Vector of 3 Doubles
 */
case class Vector(x:Double,y:Double,z:Double){
    
    /** Add Vector */
    def + (v:Vector):Vector = Vector(x+v.x,y+v.y,z+v.z)
    /** Multiply by Vector */
    def * (v:Vector):Vector = Vector(x*v.x,y*v.y,z*v.z)
    /** Vector as Sequence */
    lazy val seq = Seq(x,y,z)

}

/**
 * All zeroes vector
 */
object ZeroVector extends Vector(0,0,0)
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
    /** Middle point between two points */
    def middle(v:Vector) = Vector(x+(v.x-x)/2,y+(v.y-y)/2,z+(v.z-z)/2)
    /** Vector component's sequence paired with labels*/
    def explainseq = Seq(("x",x),("y",y),("z",z))
    /** Verbose explanation of vector */
    def explain:String = explainseq.foldLeft[String]("")((s,p) => s + p._1+"="+DoubleFormat.short(p._2)+" ")
    /** Test if equals with given coordinates */
    def equals(ex:Double = 0, ey:Double = 0, ez:Double = 0):Boolean = (x==ex && y==ey && z==ez)

}

/**
 * All zeroes vector
 */
object ZeroVector extends Vector(0,0,0)
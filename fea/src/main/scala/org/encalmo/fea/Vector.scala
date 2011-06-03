package org.encalmo.fea

case class Vector(x:Double,y:Double,z:Double) {
    
    def + (v:Vector):Vector = Vector(x+v.x,y+v.y,z+v.z)
    def * (v:Vector):Vector = Vector(x*v.x,y*v.y,z*v.z)

}

object ZeroVector extends Vector(0,0,0)
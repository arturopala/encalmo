package org.encalmo.fea

package object z88 {
    
    implicit def double3Touple2Vector(d:(Double,Double,Double)):Vector = Vector(d._1,d._2,d._3)
    
}

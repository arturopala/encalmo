package org.encalmo

package object fea {
    
    /** Sequence of doubles*/
    type OptDoubleSeq = Option[Seq[Option[Double]]]
    /** Implicit double to vector conversion */
    implicit def double3Touple2Vector(d:(Double,Double,Double)):Vector = Vector(d._1,d._2,d._3)
    
}

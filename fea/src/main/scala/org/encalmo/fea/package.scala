package org.encalmo

import scala.language.implicitConversions

package object fea {
    
    /** Sequence of doubles*/
    type OptDoubleSeq = Option[Seq[Option[Double]]]
    /** Implicit double to vector conversion */
    implicit def double3Touple2Vector(d:(Double,Double,Double)):Vector = Vector(d._1,d._2,d._3)
    
    /** Implicit element to type conversions */
    implicit def plate19Type[Plate19]:FiniteElementType = Plate19Type
    implicit def plate20Type[Plate20]:FiniteElementType = Plate20Type
    
}

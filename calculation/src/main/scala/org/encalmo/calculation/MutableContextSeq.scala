package org.encalmo.calculation

import scala.collection.mutable.{Seq,MutableList}
import scala.annotation.tailrec
import org.encalmo.expression._
import scala.collection.mutable

/** 
 * Mutable sequence of Context
 */
trait MutableContextSeq extends ContextSeq {

    protected var contexts = Vector[Context]()
    
    /** Add new nested context */
    def add(context: Context):Unit = {
        contexts = contexts :+ context
    }

}
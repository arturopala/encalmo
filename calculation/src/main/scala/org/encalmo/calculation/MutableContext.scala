package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._

/** 
 * MutableContext is an updatable and closable Context
 */
trait MutableContext extends Context {
   
  self=>  
    
  //implicit self-reference
  implicit val ctx:MutableContext = this
  
  override def map:Map[Symbol,Expression]
  
  protected var opened:Boolean = true
  /** Locks context content */
  def lock = {opened = false}  
  protected def throwContextAlreadyLockedException = throw new IllegalStateException("This context has been alredy locked.")
  
  /**
   * Maps expression to the symbol in this context
   */
  def update(s:Symbol, e:Expression):Unit = {
		if(opened){
		    val symb = symbol(s)
		    val expr:Expression = e match {
		        case v:Value => v.convertTo(s.unit)
		        case _ => e
		    }
		    map.put(symb,expr)
		} else throwContextAlreadyLockedException
  }

}
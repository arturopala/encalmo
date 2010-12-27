package org.encalmo.expression

/**
 * Operation expression
 * @author artur.opala
 *
 */
trait Operation extends Expression {
	
  def operator:String
  def precedence:Int = 100
  
}
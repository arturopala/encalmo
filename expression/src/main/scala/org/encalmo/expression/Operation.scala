package org.encalmo.expression

/**
 * Operation expression
 * @author artur.opala
 *
 */
trait Operation extends Expression {
	
  def operator:scala.Symbol
  def precedence:Int = 100
  
}
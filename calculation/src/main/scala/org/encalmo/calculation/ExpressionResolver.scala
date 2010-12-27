package org.encalmo.calculation

import org.encalmo.expression._

/** 
 * Expression resolver trait
 */
trait ExpressionResolver {
	
	private val MAX_LOOP_COUNT:Int = 128
	
	/**
	 * Resolves all symbols to mapped expressions 
	 */
	def resolve(e:Expression):Expression = {
		map(e,resolver)
	}
	
	/**
	 * Resolves and evaluates all symbols to values
	 */
	def evaluate(e:Expression):Expression = {
		map(e,evaluator)
	}
	
	def map(e1:Expression, t:Transformation, c:Int = 0):Expression = {
		val e2 = e1.map(t)
		if(c>=MAX_LOOP_COUNT){
			throw new IllegalStateException("Probably circular reference: "+e1)
		}else{
			if(e1.eq(e2)) {
				return e1
			}
		}
		map(e2,t,c+1)
	}
	
	/**
	 * Single-pass resolving transformation. 
	 * Replaces symbols with mapped expressions
	 */
	private val resolver:Transformation = {
		e => e match {
			case s:Symbol => this.getExpression(s).getOrElse(s)
			case _ => e
		}
	}
	
	/**
	 * Single-pass evaluating transformation. 
	 * Replaces symbols with values
	 */
	private val evaluator:Transformation = {
		e => e match {
			case s:Symbol => this.getExpression(s) match {
				case Some(x) => x.eval
				case None => s
			}
			case _ => e.eval
		}
	}
	
	/**
	 * Should return expression mapped to that symbol or None
	 */
	def getExpression(s:Symbol):Option[Expression]
	
	/**
	 * Should return true if exists expression mapped to that symbol
	 */
	def hasExpression(s:Symbol):Boolean
}
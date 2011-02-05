package org.encalmo.document
import scala.collection.mutable._

import org.encalmo.expression.Expression
import org.encalmo.expression.Symbol
import org.encalmo.expression.Number
import org.encalmo.expression.SymbolWithDescription
import org.encalmo.calculation.Calculation

/**
 * Assertion class
 * @author artur.opala
 */
abstract class Assertion(
		val text:String,
		val calc:Calculation, 
		val leftExpression:Expression,
		val rightExpression:Expression
) 
extends DocumentComponent(null){
	
	lazy val parentStylesConfig = parentOrSiblingOfType[StylesConfig](classOf[StylesConfig])
	
	lazy val trueStyle:Style = parentStylesConfig match {case Some(sc) => sc.assertions.iftrue.get; case None => DefaultStyle}
	lazy val falseStyle:Style= parentStylesConfig match {case Some(sc) => sc.assertions.iffalse.get; case None => DefaultStyle}
	lazy val unknownStyle:Style = parentStylesConfig match {case Some(sc) => sc.assertions.unknown.get; case None => DefaultStyle}
	
	def operator:Character
	def assert(r1:Expression,r2:Expression):Option[Boolean]
	
	override def myStyle:Style = {
		val r1 = calc.evaluate(leftExpression)
		val r2 = calc.evaluate(rightExpression)
		val ob = assert(r1,r2)
		ob match {
			case Some(b) => b match {
				case true => trueStyle
				case false => falseStyle
			}
			case None => unknownStyle
		}
	}
	
	def evaluate:Seq[DocumentComponent] = {
		val seq:ListBuffer[DocumentComponent] = ListBuffer()
		val r1 = calc.evaluate(leftExpression)
		val r2 = calc.evaluate(rightExpression)
		val ob = assert(r1,r2)
		if(text!=null){
			seq += TextToTranslate(text)
			seq += Character.SPACE
		}
		seq += TextToTranslate(ob match {
			case Some(b) => b match {
				case true => "verified"
				case false => "not_verified"
			}
			case None => "unknown"
		})
		seq += Character.SPACE
		seq += Symb(leftExpression)
		seq += Character.SPACE
		seq += operator
		seq += Character.SPACE
		seq += Symb(rightExpression)
		seq += Character.SPACE
		seq += Character.RARROW
		seq += Character.SPACE
		seq += Result(calc,leftExpression)
		seq += Character.SPACE
		seq += operator
		seq += Character.SPACE
		seq += Result(calc,rightExpression)
		seq
	}
	
}

/**
 * Assertion companion object
 * @author artur.opala
 */
object AssertionLE {
	
	def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,le,re){
		def operator:Character = Character.LE
		def assert(le:Expression,re:Expression):Option[Boolean] = (le,re) match {
			case (n1:Number,n2:Number) => {
				Some(n1 <= n2)
			}
			case _ => None
		}
	}
	
}
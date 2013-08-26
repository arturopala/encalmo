package org.encalmo.document

import scala.collection.mutable._
import org.encalmo.expression.Expression
import org.encalmo.expression.Number
import org.encalmo.calculation.{ResultsCache, Calculation}
import org.encalmo.style.StylesConfig
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle
import scala.collection.mutable

/**
 * Assertion class
 * @author artur.opala
 */
abstract class Assertion(
		val text:String,
		val calc:Calculation, 
		val expressions:mutable.Seq[Expression]
) 
extends DocumentComponent(null){
	
	lazy val parentStylesConfig:Option[StylesConfig] = document.map(_.stylesConfig)
	
	lazy val trueStyle:Style = parentStylesConfig match {case Some(sc) => sc.assert_true.get; case None => DefaultStyle}
	lazy val falseStyle:Style= parentStylesConfig match {case Some(sc) => sc.assert_false.get; case None => DefaultStyle}
	lazy val unknownStyle:Style = parentStylesConfig match {case Some(sc) => sc.assert_unknown.get; case None => DefaultStyle}
	
	def operator:mutable.Seq[Character]
	def assert(results:mutable.Seq[Expression]):Option[Boolean]
	
	/*override def myStyle:Style = {
		val results = expressions.map(calc.evaluate)
		val ob = assert(results)
		ob match {
			case Some(b) => b match {
				case true => trueStyle
				case false => falseStyle
			}
			case None => unknownStyle
		}
	}*/
	
	def evaluate(cache: ResultsCache):(Option[Boolean],mutable.Seq[DocumentComponent]) = {
		val seq:ListBuffer[DocumentComponent] = ListBuffer()
		val results = expressions.map(calc.evaluate(_)(cache))
		val ob = assert(results)
		if(text!=null){
			seq += TextToTranslate("requirement","document")
			seq += Character.SPACE
			seq += Text(text)
			seq += Character.SPACE
		}
		seq += TextToTranslate(ob match {
			case Some(b) => b match {
				case true => "verified"
				case false => "not_verified"
			}
			case None => "unknown"
		},"document")
		seq += Text(":")
		seq += Character.LONGSPACE
		seq += Symb(expressions.head)(calc)
		for(i <-1 to expressions.tail.size){
		    seq += Character.SPACE
		    seq += operator(i-1)
		    seq += Character.SPACE
		    seq += Symb(expressions(i))(calc)
		}
		seq += Character.LONGSPACE
		seq += Character.RARROW
		seq += Character.LONGSPACE
		seq += Symb(results.head)(calc)
        for(i <-1 to results.tail.size){
            seq += Character.SPACE
            seq += operator(i-1)
            seq += Character.SPACE
            seq += Symb(results(i))(calc)
        }
		(ob,seq)
	}
	
}

object AssertionLE {
	def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,re)){
		def operator:mutable.Seq[Character] = mutable.Seq(Character.LE)
		def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number) => {
				Some(n1 <= n2)
			}
			case _ => None
		}
	}
}

object AssertionGE {
    def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.GE)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number) => {
                Some(n1 >= n2)
            }
            case _ => None
        }
    }
}

object AssertionG {
    def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.GREATER)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number) => {
                Some(n1 > n2)
            }
            case _ => None
        }
    }
}

object AssertionL {
    def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.LOWER)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number) => {
                Some(n1 < n2)
            }
            case _ => None
        }
    }
}

object AssertionE {
    def apply(text:String, calc:Calculation,le:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.EQUAL)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number) => {
                Some(n1 == n2)
            }
            case _ => None
        }
    }
}

object AssertionRangeLLE {
    def apply(text:String, calc:Calculation,le:Expression,e:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,e,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.LOWER,Character.LE)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 < n2 && n2 <= n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLEL {
    def apply(text:String, calc:Calculation,le:Expression,e:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,e,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.LE,Character.LOWER)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 <= n2 && n2 < n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLL {
    def apply(text:String, calc:Calculation,le:Expression,e:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,e,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.LOWER,Character.LOWER)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 < n2 && n2 < n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLELE {
    def apply(text:String, calc:Calculation,le:Expression,e:Expression,re:Expression) = new Assertion(text,calc,mutable.Seq(le,e,re)){
        def operator:mutable.Seq[Character] = mutable.Seq(Character.LE,Character.LE)
        def assert(results:mutable.Seq[Expression]):Option[Boolean] = results match {
            case mutable.Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 <= n2 && n2 <= n3)
            }
            case _ => None
        }
    }
}
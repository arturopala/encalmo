package org.encalmo.document

import scala.collection.mutable.{ListBuffer}
import org.encalmo.expression.Expression
import org.encalmo.expression.Number
import org.encalmo.calculation.{ResultsCache, Context}
import org.encalmo.style.StylesConfig
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle

/**
 * Assertion class
 * @author artur.opala
 */
abstract class Assertion(
		val text:String,
		val expressions:Seq[Expression],
        val context: Context)
    extends DocumentComponent {
	
	lazy val parentStylesConfig:Option[StylesConfig] = document.map(_.stylesConfig)
	
	lazy val trueStyle:Style = parentStylesConfig match {case Some(sc) => sc.assert_true.get; case None => DefaultStyle}
	lazy val falseStyle:Style= parentStylesConfig match {case Some(sc) => sc.assert_false.get; case None => DefaultStyle}
	lazy val unknownStyle:Style = parentStylesConfig match {case Some(sc) => sc.assert_unknown.get; case None => DefaultStyle}
	
	def operator:Seq[Character]
	def assert(results:Seq[Expression]):Option[Boolean]
	
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
	
	def evaluate(cache: ResultsCache):(Option[Boolean],Seq[DocumentComponent]) = {
		val seq:ListBuffer[DocumentComponent] = ListBuffer()
		val results = expressions.map(context.evaluate(_)(cache))
		val ob = assert(results)
		if(text!=null){
			seq += Text("requirement","document")
			seq += Character.SPACE
			seq += Text(text)
			seq += Character.SPACE
		}
		seq += Text(ob match {
			case Some(b) => b match {
				case true => "verified"
				case false => {
                    Console.err.println("Requirement not fulfilled: "+text)
                    "not_verified"
                }
			}
			case None => "unknown"
		},"document")
		seq += Text(":")
		seq += Character.LONGSPACE
		seq += Symb(expressions.head)
		for(i <-1 to expressions.tail.size){
		    seq += Character.SPACE
		    seq += operator(i-1)
		    seq += Character.SPACE
		    seq += Symb(expressions(i))
		}
		seq += Character.LONGSPACE
		seq += Character.RARROW
		seq += Character.LONGSPACE
		seq += Symb(results.head)
        for(i <-1 to results.tail.size){
            seq += Character.SPACE
            seq += operator(i-1)
            seq += Character.SPACE
            seq += Symb(results(i))
        }
		(ob,seq)
	}
	
}

object AssertionLE {
	def apply(text:String, le:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,re),context){
		def operator:Seq[Character] = Seq(Character.LE)
		def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number) => {
				Some(n1 <= n2)
			}
			case _ => None
		}
	}
}

object AssertionGE {
    def apply(text:String, le:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,re),context){
        def operator:Seq[Character] = Seq(Character.GE)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number) => {
                Some(n1 >= n2)
            }
            case _ => None
        }
    }
}

object AssertionG {
    def apply(text:String, le:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,re),context){
        def operator:Seq[Character] = Seq(Character.GREATER)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number) => {
                Some(n1 > n2)
            }
            case _ => None
        }
    }
}

object AssertionL {
    def apply(text:String, le:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,re),context){
        def operator:Seq[Character] = Seq(Character.LOWER)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number) => {
                Some(n1 < n2)
            }
            case _ => None
        }
    }
}

object AssertionE {
    def apply(text:String, le:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,re),context){
        def operator:Seq[Character] = Seq(Character.EQUAL)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number) => {
                Some(n1 == n2)
            }
            case _ => None
        }
    }
}

object AssertionRangeLLE {
    def apply(text:String, le:Expression,e:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,e,re),context){
        def operator:Seq[Character] = Seq(Character.LOWER,Character.LE)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 < n2 && n2 <= n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLEL {
    def apply(text:String, le:Expression,e:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,e,re),context){
        def operator:Seq[Character] = Seq(Character.LE,Character.LOWER)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 <= n2 && n2 < n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLL {
    def apply(text:String, le:Expression,e:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,e,re),context){
        def operator:Seq[Character] = Seq(Character.LOWER,Character.LOWER)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 < n2 && n2 < n3)
            }
            case _ => None
        }
    }
}

object AssertionRangeLELE {
    def apply(text:String, le:Expression,e:Expression,re:Expression)(implicit context: Context) = new Assertion(text,Seq(le,e,re),context){
        def operator:Seq[Character] = Seq(Character.LE,Character.LE)
        def assert(results:Seq[Expression]):Option[Boolean] = results match {
            case Seq(n1:Number,n2:Number,n3:Number) => {
                Some(n1 <= n2 && n2 <= n3)
            }
            case _ => None
        }
    }
}
package org.encalmo.document

import scala.collection.mutable.LinkedList

import org.encalmo.expression.Expression
import org.encalmo.expression.Transformations
import org.encalmo.expression.Operation2
import org.encalmo.expression.OperationN
import org.encalmo.expression.Symbol
import org.encalmo.calculation.Calculation

/**
 * Evaluate: symbol = resolved = evaluated
 */
class Evaluate(myStyle:Style, val styleOfResolved:Style, val styleOfEvaluated:Style, val isPrintDescription:Boolean, val variant:Int, calc:Calculation, expr:Expression*) 
extends BlockExpr(myStyle,calc,expr:_*){
	
	override def toString = "Evaluate("+myStyle+","+styleOfResolved+","+styleOfEvaluated+","+calc+","+expr.mkString(",")+")"
	
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		var se = Seq[ExpressionToPrint]()
		var ue = e // unresolved expression
		var unit:String = null
		val evaluated = calc.evaluate(e) // evaluated expression
		if(e.isInstanceOf[Symbol]){
			se = se :+ ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig)
			ue = calc.getRawExpression(e.asInstanceOf[Symbol]) match {
				case Some(x) => x
				case None => e
			}
			if(ue!=e && ue!=evaluated){
				se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null,parentStylesConfig)
			}
			e match {
			    case s:Symbol if s.hasUnit => unit = s.unit.get
			    case _ => 
			}
		}else{
			if(ue!=evaluated){
				se = se :+ ExpressionToPrint(ue,resolveStyle(myStyle,StylesConfigSymbols.EXPR_UNRESOLVED),null,null,parentStylesConfig)
			}
		}
		if(evaluated!=e){
			val substituted = calc.substitute(ue) // expression with substituted symbols
			if(substituted!=ue && substituted!=evaluated){
				se = se :+ ExpressionToPrint(substituted,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_SUBSTITUTED),"=",null,parentStylesConfig)
				val depth = substituted.countTreeLeafs
				if(depth>3){
    				val evaluation1 = substituted match { // partialy evaluated expression
                        case o:Operation2 => {
                            o.copy(calc.evaluate(o.l),calc.evaluate(o.r))
                        }
                        case o:OperationN => {
                            o.copy(o.args.map(calc.evaluate(_)):_*)
                        }
                        case _ => calc.evaluate(substituted)
                    }
                    if(evaluation1!=substituted && evaluation1!=evaluated){
                        se = se :+ ExpressionToPrint(evaluation1,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED),"=",null,parentStylesConfig)
                    }
				}
			}
			se = se :+ ExpressionToPrint(evaluated,resolveStyle(styleOfEvaluated,StylesConfigSymbols.EXPR_EVALUATED),"=",unit,parentStylesConfig)
		}
		se
	}
}

/**
 * Evaluate: symbol = resolved = evaluated
 * @author artur.opala
 */
object Evaluate {
	
	def apply(mystyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,styleOfResolved,styleOfEvaluated,true,1,calc,expr:_*)
	}
	
	def apply(mystyle:Style, style2:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,style2,style2,true,1,calc,expr:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expr:Expression*) = {
		new Evaluate(mystyle,mystyle,mystyle,true,1,calc,expr:_*)
	}
	
	def apply(calc:Calculation, expr:Expression*) = {
        new Evaluate(null,null,null,true,1,calc,expr:_*)
    }
	
	def apply(expr:Seq[Expression],calc:Calculation) = {
        new Evaluate(null,null,null,true,1,calc,expr:_*)
    }
	
	def apply(isPrintDescription:Boolean, calc:Calculation, expr:Expression*) = {
		new Evaluate(null,null,null,isPrintDescription,1,calc,expr:_*)
	}
	
	def apply(isPrintDescription:Boolean, variant:Int, calc:Calculation, expr:Expression*) = {
		new Evaluate(null,null,null,isPrintDescription,variant,calc,expr:_*)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.calc,e.expr)
	
}
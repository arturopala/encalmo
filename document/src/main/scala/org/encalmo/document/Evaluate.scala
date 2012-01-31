package org.encalmo.document

import scala.collection.mutable.LinkedList
import org.encalmo.expression.Expression
import org.encalmo.expression.Transformations
import org.encalmo.expression.Operation2
import org.encalmo.expression.OperationN
import org.encalmo.expression.Symbol
import org.encalmo.expression.SymbolLike
import org.encalmo.expression.Selection
import org.encalmo.expression.UnitOfValue
import org.encalmo.expression.Function
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.BoundExpression
import org.encalmo.calculation.EvalAt
import org.encalmo.style.Style
import org.encalmo.style.StylesConfigSymbols
import org.encalmo.expression.Transparent
import org.encalmo.expression.UnitOfValue
import org.encalmo.expression.EmptyUnitOfValue
import org.encalmo.expression.Sum
import org.encalmo.expression.Value

/**
 * Evaluate: symbol = resolved = evaluated
 */
class Evaluate(myStyle:Style, val styleOfResolved:Style, val styleOfEvaluated:Style, val isPrintDescription:Boolean, val variant:Int, calc:Calculation, expr:Expression*) 
extends BlockExpr(myStyle,calc,expr:_*){
	
	override def toString = "Evaluate("+myStyle+","+styleOfResolved+","+styleOfEvaluated+","+calc+","+expr.mkString(",")+")"
	
	override def resolveExpression(e:Expression):Seq[ExpressionToPrint] = {
		var se = Seq[ExpressionToPrint]()
		var ue = e // unresolved expression
		val unit:UnitOfValue = e.unit
		val evaluated = adjustUnitsInSum(calc.evaluate(e),unit) // evaluated expression
		e match {
			case future:BoundExpression => {
				se = se :+ ExpressionToPrint(future.symbol,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig)
				ue = future.er.getRawExpression(future.symbol) match {
					case Some(x) => x
					case None => e
				}
				if(ue!=e && ue!=evaluated){
					se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null,parentStylesConfig)
				}
			}
			case symbol:Symbol => {
				ue = prepareUnresolved(symbol, calc.getRawExpression(symbol) match {
					case Some(x) => x
					case None => e
				})
				se = se :+ ExpressionToPrint(e,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig)
				if(ue!=e && ue!=evaluated){
				    se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null,parentStylesConfig)
				}
			}
			case _ => {
				if(ue!=evaluated){
					se = se :+ ExpressionToPrint(ue,resolveStyle(myStyle,StylesConfigSymbols.EXPR_UNRESOLVED),null,null,parentStylesConfig)
				}
			}
		}
		
		if(evaluated!=e){
			val substituted = adjustUnitsInSum(calc.substitute(ue),unit) // expression with substituted symbols
			if(substituted!=ue && substituted!=evaluated){
				se = se :+ ExpressionToPrint(substituted,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_SUBSTITUTED),"=",null,parentStylesConfig)
				val depth = substituted.countTreeLeafs
				if(depth>3 || substituted.isInstanceOf[Selection]){
    				val evaluation1 = adjustUnitsInSum((substituted match {
    				    case tr:Transparent => tr.children.first
    				    case x => x
    				}) match { // partialy evaluated expression
    				    case null => substituted
                        case o:Operation2 => {
                            o.copy(calc.evaluate(o.l),calc.evaluate(o.r))
                        }
                        case o:OperationN => {
                            o.copy(o.args.map(calc.evaluate(_)):_*)
                        }
                        case sel:Selection => {
                        	calc.substitute(sel.select)
                        }
                        case _ => calc.evaluate(substituted)
                    },unit)
                    if(evaluation1!=substituted && evaluation1!=evaluated){
                        se = se :+ ExpressionToPrint(evaluation1,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED),"=",null,parentStylesConfig)
                    }
				}
			}
			se = se :+ ExpressionToPrint(evaluated,resolveStyle(styleOfEvaluated,StylesConfigSymbols.EXPR_EVALUATED),"=",null,parentStylesConfig)
		}
		se
	}
	
	def prepareUnresolved(symbol:Symbol,e:Expression) = {
	    e match {
	        case ev:EvalAt => EvalAt(ev.expr, ev.er.evaluateWithAndReturnCopy(calc))
	        case _ => e
	    }
	}
	
	def adjustUnitsInSum(e:Expression, unit:UnitOfValue):Expression = unit match {
	    case EmptyUnitOfValue => e 
	    case u => e match {
    	    case s:Sum => if(s.args.exists(_.isInstanceOf[Value])) Sum(s.args.map(b => b match {
    	        case v:Value => v.convertTo(u) 
    	        case _ => b
	        }):_*) else s
    	    case _ => e
	}}
	
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
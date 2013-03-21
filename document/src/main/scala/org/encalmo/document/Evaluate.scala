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
import org.encalmo.expression.max
import org.encalmo.expression.min
import org.encalmo.expression.Value
import org.encalmo.expression.Number
import org.encalmo.calculation.DynamicExpression
import org.encalmo.calculation.ExpressionResolver
import org.encalmo.calculation.Context

/**
 * Evaluate: symbol = unresolved = resolved = substituted = evaluated
 */
class Evaluate(
        myStyle:Style, 
        val styleOfResolved:Style, 
        val styleOfEvaluated:Style, 
        val isPrintDescription:Boolean, 
        context:Context, 
        expressions:Expression*) 
extends BlockExpr(myStyle,context,expressions:_*){
	
	override def toString = "Evaluate("+myStyle+","+styleOfResolved+","+styleOfEvaluated+","+context+","+expressions.mkString(",")+")"
	
	override def prepareExpressionToPrint(e:Expression):Seq[ExpressionToPrint] = {
		var se = Seq[ExpressionToPrint]()
		var ue = e // unresolved expression
		val unit:UnitOfValue = e.unit
		val evaluated = adjustUnitsInSum(context.evaluate(e),unit) // evaluated expression
		e match {
			case boundexpr:BoundExpression => {
				se = se :+ ExpressionToPrint(boundexpr.symbol,resolveStyle(myStyle,StylesConfigSymbols.EXPR_SYMBOL),null,null,parentStylesConfig)
				ue = processUnresolved(boundexpr.symbol,boundexpr.er.getRawExpression(boundexpr.symbol) match {
					case Some(x) => x
					case None => e
				})
				if(ue!=e && ue!=evaluated){
					se = se :+ ExpressionToPrint(ue,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_UNRESOLVED),"=",null,parentStylesConfig)
				}
			}
			case symbol:Symbol => {
				ue = processUnresolved(symbol, context.getRawExpression(symbol) match {
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
			val substituted = adjustUnitsInSum(context.substitute(ue),unit) // expression with substituted symbols
			if(substituted!=ue && substituted!=evaluated){
				se = se :+ ExpressionToPrint(substituted,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_SUBSTITUTED),"=",null,parentStylesConfig)
				val depth = substituted.countTreeLeafs
				if(depth>3 || substituted.isInstanceOf[Selection]){
    				val evaluation1 = adjustUnitsInSum((substituted match {
    				    case tr:Transparent => tr.children.head
    				    case x => x
    				}) match { // partialy evaluated expression
    				    case null => substituted
                        case o:Operation2 => {
                            o.copy(context.evaluate(o.l),context.evaluate(o.r))
                        }
                        case o:OperationN => {
                            o.copy(o.args.map(context.evaluate(_)):_*)
                        }
                        case sel:Selection => {
                        	context.substitute(sel.select)
                        }
                        case _ => context.evaluate(substituted)
                    },unit)
                    if(evaluation1!=substituted && evaluation1!=evaluated){
                        se = se :+ ExpressionToPrint(evaluation1,resolveStyle(styleOfResolved,StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED),"=",null,parentStylesConfig)
                    }
				}
			}
			val evalchar = evaluated match {
			    case n:Number => if(!n.isRounded) "=" else "≈"
			    case _=> "="
			}
			se = se :+ ExpressionToPrint(evaluated,resolveStyle(styleOfEvaluated,StylesConfigSymbols.EXPR_EVALUATED),evalchar,null,parentStylesConfig)
		}
		se
	}
	
	def processUnresolved(symbol:Symbol,e:Expression):Expression = {
	    e match {
	        case ev:EvalAt => EvalAt(ev.expr, ev.er.evaluateWithAndReturnCopy(context))
	        case de:DynamicExpression => de.f()
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
    	    case s:min => if(s.args.exists(_.isInstanceOf[Value])) min(s.args.map(b => b match {
                case v:Value => v.convertTo(u) 
                case _ => b
            }):_*) else s
            case s:max => if(s.args.exists(_.isInstanceOf[Value])) max(s.args.map(b => b match {
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
	
	def apply(mystyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(mystyle,styleOfResolved,styleOfEvaluated,true,calc,expressions:_*)
	}
	
	def apply(mystyle:Style, style2:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(mystyle,style2,style2,true,calc,expressions:_*)
	}
	
	def apply(mystyle:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(mystyle,mystyle,mystyle,true,calc,expressions:_*)
	}
	
	/*def apply(calc:Calculation, expressions:Expression*) = {
        new Evaluate(null,null,null,true,calc,expressions:_*)
    }*/
	
	def apply(expressions:Expression*)(implicit context:Context) = {
        new Evaluate(null,null,null,true,context,expressions:_*)
    }
	
	def apply(isPrintDescription:Boolean, calc:Calculation, expressions:Expression*) = {
		new Evaluate(null,null,null,isPrintDescription,calc,expressions:_*)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
	
}
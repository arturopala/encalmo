package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.calculation._
import scala.Some
import org.encalmo.style.Style

/**
 * Evaluate: symbol = unresolved = substituted = partially evaluated = evaluated
 */
class Evaluate(
        customStyle:Style,
        val styleOfResolved:Style, 
        val styleOfEvaluated:Style, 
        val isPrintDescription:Boolean, 
        context:Context, 
        expressions:Expression*) 
extends BlockExpr(customStyle,context,expressions:_*){
	
	override def toString = "Evaluate("+customStyle+","+styleOfResolved+","+styleOfEvaluated+","+context+","+expressions.mkString(",")+")"
	
	/*override def prepareExpressionToPrint(e:Expression, formulaSetCache: FormulaSetCache):Seq[ExpressionToPrint] = {
		val toPrints = scala.collection.mutable.Seq[ExpressionToPrint]()
        formulaSetCache.formulaSetFor(context).get(e)
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
		toPrints
	}*/
	
}

/**
 * Evaluate: symbol = resolved = evaluated
 * @author artur.opala
 */
object Evaluate {
	
	def apply(customStyle:Style, styleOfResolved:Style, styleOfEvaluated:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,styleOfResolved,styleOfEvaluated,true,calc,expressions:_*)
	}
	
	def apply(customStyle:Style, style2:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,style2,style2,true,calc,expressions:_*)
	}
	
	def apply(customStyle:Style, calc:Calculation, expressions:Expression*) = {
		new Evaluate(customStyle,customStyle,customStyle,true,calc,expressions:_*)
	}
	
	def apply(expressions:Expression*)(implicit context:Context) = {
        new Evaluate(null,null,null,true,context,expressions:_*)
    }
	
	def apply(isPrintDescription:Boolean, calc:Calculation, expressions:Expression*) = {
		new Evaluate(null,null,null,isPrintDescription,calc,expressions:_*)
	}
	
	def unapply(e:Evaluate) = Some(e.myStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
	
}
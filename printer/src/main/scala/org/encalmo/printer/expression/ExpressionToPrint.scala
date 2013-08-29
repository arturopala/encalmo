package org.encalmo.printer.expression

import org.encalmo.expression._
import org.encalmo.style.StylesConfigSymbols
import org.encalmo.calculation._
import org.encalmo.document._
import scala.collection.mutable.ArrayBuffer
import org.encalmo.style.Style
import org.encalmo.calculation.Formula
import org.encalmo.calculation.FormulaPart
import org.encalmo.style.StylesConfig

/**
 * Wrapped expression prepared to print
 * @author artur.opala
 */
case class ExpressionToPrint(
	expression:Expression,
	style:Style,
	prefix:String,
	suffix:String,
	stylesConfig:Option[StylesConfig] = None
){
    /** style's classId or standard id from styles config */
    def styleClassId:Option[String] = if(style!=null) stylesConfig.map(_.matchStyleClassId(style).getOrElse({style.classId})) else None
    
}

object ExpressionToPrint {

    def prepare(element: Expr, results: Results):Seq[Seq[ExpressionToPrint]] = {
        for(expression <- element.expressions) yield prepare(expression, element, results)
    }

    def prepare(expression:Expression, element: Expr, results: Results):Seq[ExpressionToPrint] = {
        val formula = expression match {
            case pinnedExpression:PinnedExpression => {
                results.formulaSet.getOrReckon(pinnedExpression.symbol,pinnedExpression.context, results)
            }
            case _ => {
                results.formulaSet.getOrReckon(expression,element.context, results)
            }
        }
        prepare(formula, partFilterForElement(element),  element.customStyle, element)
    }

    val ALL_PARTS: FormulaPart => Boolean = {part => true}
    val ONLY_LEFT: FormulaPart => Boolean = {part => part.position == FormulaPosition.LEFT}
    val ONLY_RIGHT: FormulaPart => Boolean = {part => part.position == FormulaPosition.RIGHT}
    val ONLY_LEFT_AND_UNRESOLVED: FormulaPart => Boolean = {part => part.position == FormulaPosition.LEFT || part.position == FormulaPosition.EXPR_UNRESOLVED}

    private def partFilterForElement(element: Expr): FormulaPart => Boolean = {
        element match {
            case _:Evaluate => ALL_PARTS
            case _:Symb => ONLY_LEFT
            case _:Result => ONLY_RIGHT
            case _:Expand => ONLY_LEFT_AND_UNRESOLVED
        }
    }

    def prepare(formula: Formula, positionFilter: FormulaPart => Boolean, customStyle: Style, stylesResolver: StylesResolver):Seq[ExpressionToPrint] = {
        formula.parts filter positionFilter match {
            case parts if parts.isEmpty => Seq.empty[ExpressionToPrint]
            case parts if parts.size==1 => Seq(ExpressionToPrint(parts.head.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(parts.head.position)),null,null,stylesResolver.parentStylesConfig))
            case parts => {
                val toPrint = ArrayBuffer[ExpressionToPrint]()
                toPrint += ExpressionToPrint(parts.head.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(parts.head.position)),null,null,stylesResolver.parentStylesConfig)
                for(part <- parts.tail){
                    toPrint += ExpressionToPrint(part.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(part.position)),prefixForPartRelation(part.relation),null,stylesResolver.parentStylesConfig)
                }
                toPrint
            }
        }
    }

    private def styleSymbolForPartPosition(position: FormulaPosition.Value): StylesConfigSymbols.Value = {
        position match {
            case FormulaPosition.LEFT => StylesConfigSymbols.EXPR_SYMBOL
            case FormulaPosition.EXPR_UNRESOLVED => StylesConfigSymbols.EXPR_UNRESOLVED
            case FormulaPosition.EXPR_SUBSTITUTED => StylesConfigSymbols.EXPR_SUBSTITUTED
            case FormulaPosition.EXPR_PARTIALLY_EVALUATED => StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED
            case FormulaPosition.RIGHT => StylesConfigSymbols.EXPR_EVALUATED
        }
    }

    private def prefixForPartRelation(relation: FormulaPartRelation.Value): String = {
        relation match {
            case FormulaPartRelation.APPROXIMATELY_EQUAL => "≈"
            case FormulaPartRelation.ASYMPTOTICALLY_EQUAL => "≃"
            case FormulaPartRelation.EQUAL => "="
            case FormulaPartRelation.GREATER => ">"
            case FormulaPartRelation.GREATER_OR_EQUAL => ">="
            case FormulaPartRelation.IDENTICAL => "="
            case FormulaPartRelation.LESS => "<"
            case FormulaPartRelation.LESS_OR_EQUAL => "<="
            case FormulaPartRelation.LIMIT => "->"
            case FormulaPartRelation.MUCH_GREATER => ">>"
            case FormulaPartRelation.MUCH_LESS => "<<"
            case FormulaPartRelation.NONE => null
            case FormulaPartRelation.PROPORTIONAL => "~"
        }
    }
}
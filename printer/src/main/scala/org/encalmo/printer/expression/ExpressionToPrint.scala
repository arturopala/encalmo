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

case class FormulaToPrint(
   expression: Expression,
   expressionsToPrint: Seq[ExpressionToPrint],
   printStyle: FormulaPrintStyle.Value = FormulaPrintStyle.NORMAL
) extends Traversable[ExpressionToPrint] {
	def foreach[U](f: (ExpressionToPrint) => U): Unit = expressionsToPrint.foreach(f)
	def apply(i: Int): ExpressionToPrint = expressionsToPrint(i)
}

object FormulaPrintStyle extends Enumeration {
	type FormulaPrintStyle = Value
	val NORMAL,BOLD,ERROR = Value
}

/**
 * Wrapped expression prepared to print
 * @author artur.opala
 */
case class ExpressionToPrint(
	expression:Expression,
	style:Style,
	prefix:String,
	suffix:String,
	stylesConfig:StylesConfig
){
    /** style's classId or standard id from styles config */
    def styleClassId:Option[String] = if(style!=null) Option(stylesConfig.matchStyleClassId(style).getOrElse(style.classId)) else None

}

object ExpressionToPrint {

    def prepare(element: Expr, results: Results, printStyle: FormulaPrintStyle.Value = FormulaPrintStyle.NORMAL):Seq[FormulaToPrint] = {
        for(expression <- element.expressions) yield prepare(expression, element, results, printStyle)
    }

    def prepare(expression:Expression, element: Expr, results: Results, printStyle: FormulaPrintStyle.Value):FormulaToPrint = {
        val formula = expression match {
            case pinnedExpression:PinnedExpression => {
                results.formulaSet.getOrReckon(pinnedExpression.symbol,pinnedExpression.context, results)
            }
            case _ => {
                results.formulaSet.getOrReckon(expression,element.context, results)
            }
        }
        val expressions = prepare(formula, filterForElement(element, formula),  element.customStyle, element)
	    FormulaToPrint(expression,expressions,printStyle)
    }

    val ALL_PARTS: FormulaPart => Boolean = {part => true}
    val ONLY_LEFT: FormulaPart => Boolean = {part => part.position == FormulaPosition.LEFT}
    val ONLY_RIGHT: FormulaPart => Boolean = {part => part.position == FormulaPosition.RIGHT}
    val ONLY_LEFT_AND_UNRESOLVED: FormulaPart => Boolean = {part => part.position == FormulaPosition.LEFT || part.position == FormulaPosition.EXPR_UNRESOLVED}
    val NOT_RIGHT: FormulaPart => Boolean = {part => part.position != FormulaPosition.RIGHT}
    val NOT_LEFT: FormulaPart => Boolean = {part => part.position != FormulaPosition.LEFT}
    val NOT_RIGHT_NOR_LEFT: FormulaPart => Boolean = {part => part.position != FormulaPosition.RIGHT && part.position != FormulaPosition.LEFT}

    def filterForElement(element: Expr, formula: Formula): FormulaPart => Boolean = {
        element match {
            case _:Evaluate => ALL_PARTS
            case _:Result => ONLY_RIGHT
            case _:Expand => ONLY_LEFT_AND_UNRESOLVED
            case _:Require => formula.result match {
                case FALSE => NOT_LEFT
                case _ => NOT_RIGHT_NOR_LEFT
            }
            case _ => ALL_PARTS
        }
    }

    def prepare(formula: Formula, positionFilter: FormulaPart => Boolean, customStyle: Style, stylesResolver: StylesResolver):Seq[ExpressionToPrint] = {
        formula.parts filter positionFilter match {
            case parts if parts.isEmpty => Seq.empty[ExpressionToPrint]
            case parts if parts.size==1 => Seq(ExpressionToPrint(parts.head.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(parts.head.position)),null,null,stylesResolver.stylesConfig))
            case parts => {
                val toPrint = ArrayBuffer[ExpressionToPrint]()
                toPrint += ExpressionToPrint(parts.head.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(parts.head.position)),null,null,stylesResolver.stylesConfig)
                for(part <- parts.tail){
                    toPrint += ExpressionToPrint(part.expression, stylesResolver.resolveExpressionStyle(customStyle, styleSymbolForPartPosition(part.position)),prefixForPartRelation(part.relation),null,stylesResolver.stylesConfig)
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
            case _ => StylesConfigSymbols.DEFAULT
        }
    }

    private def prefixForPartRelation(relation: Relation.Value): String = Relation.faceOf(relation)
}
package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document._
import org.encalmo.calculation.{Context, Results}
import org.encalmo.printer.expression._
import scala.collection.mutable
import org.encalmo.style.{DefaultStyle, Style}
import org.encalmo.expression._
import java.text.DecimalFormat
import org.encalmo.printer.expression.FormulaToPrint
import scala.Some
import org.encalmo.expression.{Assert,Number}
import org.encalmo.common.Node

/**
 * DocumentPrinter trait
 * A - output type
 * B - content type
 * @author artur.opala
 */
trait DocumentPrinter[A<:Output[B],B] extends Printer[Document,A,B,Results]

/**
 * DocumentPrinter visitor trait
 */
trait DocumentPrinterVisitor {

    def counter: Option[MultiCounter]
    def output: TextOutput

    val locale = output.locale
    val mathOutput = output.toMathMLOutput
    val ept = new MathMLExpressionPrinterVisitor(mathOutput)
    val shortDecimalFormat: DecimalFormat = new DecimalFormat("#.#",output.decimalFormatSymbols)

    var closestNumSection: Option[NumSection] = None

    /** Section counters map */
    val counterMap:mutable.LinkedHashMap[Enumerator,MultiCounter] = mutable.LinkedHashMap[Enumerator,MultiCounter]()
    counter.foreach(c => counterMap.put(c.enumerator,c))

    val styleStack:mutable.Stack[Style] = mutable.Stack()
    styleStack.push(DefaultStyle)

    /** Returns counter linked to the enumerator */
    def counterFor(en:Enumerator):MultiCounter = {
        counterMap.getOrElseUpdate(en,MultiCounter(en))
    }

    def printStyleOfCheck(expression: Expression, results: Results): FormulaPrintStyle.Value = {
        expression match {
            case s:Symbol if s.name.startsWith(Context.REQUIREMENT_SYMBOL_PREFIX) => results.cache.get(s) map {
                case TRUE => FormulaPrintStyle.BOLD
                case FALSE => FormulaPrintStyle.ERROR
                case _ => FormulaPrintStyle.NORMAL
            } getOrElse FormulaPrintStyle.NORMAL
            case _ => FormulaPrintStyle.NORMAL
        }
    }

    def concatenate(args: Option[String]*): Option[String] = {
        args.fold(None)((ac,v) => ac match {
            case None => v
            case some => v match {
                case None => some
                case Some(d) => some.map(_ +" "+d)
            }
        })
    }

    def ratioOfAssertFormula(expressions: Seq[ExpressionToPrint]): String = {
        expressions(expressions.size-2).expression match {
            case a:Assert => a.ratio.eval() match {
                case n:Number => shortDecimalFormat.format(n.toDouble) + "% - "
                case _ => ""
            }
            case _ => ""
        }
    }

}

/** Expression print strategy */
trait ExpressionPrintStrategy {
    def print(node:Node[DocumentComponent], ess:Seq[FormulaToPrint], isPrintDescription: Boolean)
}
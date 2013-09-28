package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document._
import org.encalmo.calculation.Results
import org.encalmo.common.Node
import org.encalmo.printer.expression.{MathMLExpressionPrinterVisitor, FormulaPrintStyle, FormulaToPrint}
import scala.collection.mutable
import org.encalmo.style.{DefaultStyle, Style}
import org.encalmo.common.Node
import org.encalmo.expression.{FALSE, TRUE, Symbol, Expression}
import org.encalmo.printer.expression.FormulaToPrint
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

    /** Section counters map */
    val counterMap:mutable.LinkedHashMap[Enumerator,MultiCounter] = mutable.LinkedHashMap[Enumerator,MultiCounter]()
    counter.foreach(c => counterMap.put(c.enumerator,c))

    val styleStack:mutable.Stack[Style] = mutable.Stack()
    styleStack.push(DefaultStyle)

    /** Returns counter linked to the enumerator */
    def counterFor(en:Enumerator):MultiCounter = {
        counterMap.getOrElseUpdate(en,MultiCounter(en))
    }

    def printStyleOfRequire(expression: Expression, results: Results): FormulaPrintStyle.Value = {
        expression match {
            case s:Symbol => results.cache.get(s) map {
                case TRUE => FormulaPrintStyle.BOLD
                case FALSE => FormulaPrintStyle.ERROR
                case _ => FormulaPrintStyle.NORMAL
            } getOrElse FormulaPrintStyle.NORMAL
            case _ => FormulaPrintStyle.NORMAL
        }
    }

    var closestNumSection: Option[NumSection] = None

}

/** Expression print strategy */
trait ExpressionPrintStrategy {
    def print(node:Node[DocumentComponent], ess:Seq[FormulaToPrint], isPrintDescription: Boolean)
}
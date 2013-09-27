package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document.{BlockExpr, DocumentComponent, Document}
import org.encalmo.calculation.Results
import org.encalmo.common.Node
import org.encalmo.printer.expression.FormulaToPrint

/**
 * Document printer trait
 * A - output type
 * B - content type
 * @author artur.opala
 */
trait DocumentPrinter[A<:Output[B],B] extends Printer[Document,A,B,Results]


/** Expression print strategy */
trait ExpressionPrintStrategy {
    def print(node:Node[DocumentComponent],expr:BlockExpr,ess:Seq[FormulaToPrint])
}
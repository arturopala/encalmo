package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document.Document
import org.encalmo.calculation.Results

/**
 * Document printer trait
 * A - output type
 * B - content type
 * @author artur.opala
 */
trait DocumentPrinter[A<:Output[B],B] extends Printer[Document,A,B,Results]
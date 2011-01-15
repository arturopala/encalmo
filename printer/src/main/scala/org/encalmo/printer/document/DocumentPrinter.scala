package org.encalmo.printer.document

import org.encalmo.printer._
import org.encalmo.document.Document

/**
 * Document printer trait
 * A - output type
 * B - result type
 * @author artur.opala
 */
trait DocumentPrinter[A<:Output[B],B] extends Printer[Document,A,B]
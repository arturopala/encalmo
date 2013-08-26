package org.encalmo.printer.expression

import org.encalmo.printer._
import org.encalmo.expression._

/**
 * Expression printer trait
 * A - output type
 * B - result type
 * @author artur.opala
 */
trait ExpressionPrinter[A<:Output[B],B] extends Printer[Expression,A,B,AnyRef]
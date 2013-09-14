package org.encalmo.expression

/**
 * Prefix operation expression
 * @author artur.opala
 *
 */
trait PostfixOperation extends Operation1 with PrimitiveOperation {

    override def face = "("+expression.face+")"+operator.name
}
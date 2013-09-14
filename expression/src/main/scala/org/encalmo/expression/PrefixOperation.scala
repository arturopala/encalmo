package org.encalmo.expression

/**
 * Prefix operation expression
 * @author artur.opala
 */
trait PrefixOperation extends Operation1 with PrimitiveOperation {

    override def face = operator.name+"("+expression.face+")"
}
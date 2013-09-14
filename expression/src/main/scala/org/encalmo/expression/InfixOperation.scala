package org.encalmo.expression

/**
 * Infix operation expression
 * @author artur.opala
 */
trait InfixOperation extends Operation2 with PrimitiveOperation {

    override def face = "(" + l.face + " " + operator.name + " "  + r.face + ")"

}

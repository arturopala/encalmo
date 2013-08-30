package org.encalmo.expression

/**
 * Trait of expressions transparently encapsulating other expressions
 * @author artur
 */
trait Transparent extends Expression {

    def expression: Expression

    def wrap(e: Expression): Transparent
}
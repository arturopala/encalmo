package org.encalmo.expression

sealed abstract class BooleanValue(val boolean: Boolean) extends Value {
    val typeId = BooleanValue.typeId
}

object BooleanValue {
    val typeId = 'Boolean
    def apply(b: Boolean): BooleanValue = if(b) TRUE else FALSE
    def unapply(b: BooleanValue): Option[Boolean] = Some(b.boolean)

    BooleanValueCalculator.doRegister()
}

object TRUE extends BooleanValue(true)
object FALSE extends BooleanValue(false)

object BooleanValueCalculator extends ValueCalculator {

    import scala.Symbol

    /**
     * Self register as a calculator
     */
    def doRegister() {
        Value.register((BooleanValue.typeId,BooleanValue.typeId),this)
    }

    /**
     * Calculates result of the operation with single argument.
     */
    def calculate(operator: Symbol, v: Value): Option[Value] = v match {
        case BooleanValue(b) => operator match {
            case '! => Some(BooleanValue(!b))
            case _ => None
        }
        case _ => None
    }

    /**
     * Calculates result of the operation with two arguments.
     */
    def calculate(operator: Symbol, v1: Value, v2: Value): Option[Value] = (v1,v2) match {
        case (BooleanValue(b1),BooleanValue(b2)) => operator match {
            case '& => Some(BooleanValue(b1 & b2))
            case '| => Some(BooleanValue(b1 | b2))
            case _ => None
        }
        case _ => None
    }
}


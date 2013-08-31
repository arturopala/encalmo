package org.encalmo.expression

/**
 * Special purpose expression object meaning unknown expression
 * @author artur.opala
 */
object Unknown extends Value {

  val typeId = 'Unknown

  def dump(w: java.io.PrintWriter): Unit = w.print("unknown")
  
  override def + (e:Expression):Expression = Unknown
  
  override def - (e:Expression):Expression = Unknown
  
  override def * (e:Expression):Expression = Unknown
  
  override def / (e:Expression):Expression = Unknown
  
  override def % (e:Expression):Expression = Unknown
  
  override def ^ (e:Expression):Expression = Unknown
  
  override def unary_-():Expression = Unknown
  
  override def +- (e:Expression):Expression = Unknown
  
  override def *- (e:Expression):Expression = Unknown
  
  override def /- (e:Expression):Expression = Unknown
  
  override def %- (e:Expression):Expression = Unknown
  
  override def ^- (e:Expression):Expression = Unknown

  UknownValueCalculator.doRegister()

}

object UknownValueCalculator extends ValueCalculator {

    import scala.Symbol

    /**
     * Self register as a calculator
     */
    def doRegister() {
        Value.register((Unknown.typeId,'*), this)
        Value.register(('*,Unknown.typeId), this)
    }

    /**
     * Calculates result of the operation with single argument.
     */
    def calculate(operator: Symbol, v: Value): Option[Value] = Some(Unknown)


    /**
     * Calculates result of the operation with two arguments.
     */
    def calculate(operator: Symbol, v1: Value, v2: Value): Option[Value] = Some(Unknown)
}
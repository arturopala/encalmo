package org.encalmo.expression

import scala.collection.mutable._

/**
 * Subtype of Expression representing value.
 * Value's eval() function always returns itself.
 * @author artur.opala
 */
trait Value extends Expression {
	
	override final def eval():Expression = this
	
	/**
	 * Unique id of the value type, as registered with ValueCalculator
	 */
	def typeId:String
	
	/**
	 * Converts this value to the new units
	 */
	def convertTo(newunit:UnitOfValue):Value = this
	
	/**
     * Converts this value to the new units with the given acuracy
     */
    def convertTo(newunit:UnitOfValue, accuracy:Option[Double]):Value = this
	
	/**
     * Converts this value to the base units
     */
	def convertToBaseUnit:Value = this
}

/**
 * Value calculator
 */
object Value {
    
    private val registry:scala.collection.mutable.Map[(String,String),ValueCalculator] = Map()
    
    def register(key:(String,String),calculator:ValueCalculator):Unit = registry.put(key,calculator)
    
    def calculate(operator:String, v1:Value,v2:Value):Option[Value] = {
        registry.get((v1.typeId,v2.typeId)).map(_.calculate(operator,v1,v2)).getOrElse(None)
    }
    
    def calculate(operator:String, v:Value):Option[Value] = {
        registry.get((v.typeId,v.typeId)).map(_.calculate(operator,v)).getOrElse(None)
    }
    
}

trait ValueCalculator {
    
    /**
     * Self register as a calculator
     */
    def doRegister:Unit
    
    /** 
     * Calculates result of the operation with single argument. 
     */
    def calculate(operator:String, v:Value):Option[Value]
    
    /** 
     * Calculates result of the operation with two arguments. 
     */
    def calculate(operator:String, v1:Value,v2:Value):Option[Value]
    
}

abstract class AbstractValueCalculator[A <: Value, B <: Value] extends ValueCalculator {
    
    private val registry1:scala.collection.mutable.Map[String,Function1[A,Value]] = Map()
    private val registry2:scala.collection.mutable.Map[String,Function2[A,B,Value]] = Map()
    
    def register(operator:String,calculator:Function1[A,Value]) = registry1.put(operator,calculator)
    def register(operator:String,calculator:Function2[A,B,Value]) = registry2.put(operator,calculator)
    
    override final def calculate(operator:String, v:Value):Option[Value] = {
        registry1.get(operator).map(_(v.asInstanceOf[A]))
    }
    
    override final def calculate(operator:String, v1:Value,v2:Value):Option[Value] = {
        registry2.get(operator).map(_(v1.asInstanceOf[A],v2.asInstanceOf[B]))
    }
    
}

class IllegalValueConversionException(message: String) extends Exception(message)
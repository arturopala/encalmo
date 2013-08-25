package org.encalmo.expression

import scala.collection.mutable._
import scala.collection.mutable

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
    def convertTo(accuracy:Option[Double]):Value = this
	
	/**
     * Converts this value to the new units with the given acuracy
     */
    def convertTo(newunit:UnitOfValue, accuracy:Option[Double]):Value = this

    /**
     * Tries convert this value to the new units with the given acuracy
     */
    def convertIfPossibleTo(newunit:UnitOfValue, accuracy:Option[Double]):Value = {
        newunit match {
            case u:UnitOfValue if this.unit.isSameBaseAndDimension(u) => convertTo(newunit, accuracy)
            case u:UnitOfValue if this.unit.isSameExpandedUnit(u) => {
                convertTo(newunit, accuracy)
            }
            case _ => this
        }
    }
	
	/**
     * Converts this value to the base units
     */
	def convertToBaseUnit:Value = this

    /**
     * Sets new unit without conversion
     */
    def setUnit(newunit:UnitOfValue) = this
}

/**
 * Value calculator
 */
object Value {
    
    private val registry:scala.collection.mutable.Map[(String,String),ValueCalculator] = mutable.Map()
    
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
    def doRegister():Unit
    
    /** 
     * Calculates result of the operation with single argument. 
     */
    def calculate(operator:String, v:Value):Option[Value]
    
    /** 
     * Calculates result of the operation with two arguments. 
     */
    def calculate(operator:String, v1:Value,v2:Value):Option[Value]
    
}

class IllegalValueConversionException(message: String) extends Exception(message)
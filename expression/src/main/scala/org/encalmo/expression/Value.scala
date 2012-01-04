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
	 * Calculates result of the operation with single argument. 
	 */
	def calculate(operator:String, v:Value):Option[Value]
	
	/** 
	 * Calculates result of the operation with two arguments. 
	 */
	def calculate(operator:String, v1:Value,v2:Value):Option[Value]
	
	/**
	 * Converts this value to the new units
	 */
	def convertTo(newunit:UnitOfValue):Value = this
	
	/**
     * Converts this value to the base units
     */
	def convertToBaseUnit:Value = this
}

object Value {
    
    type Operator1 = (Value) => Value
    type Operator2 = (Value,Value) => Value
    
    private val registry1:Map[Tuple2[String,Class[_]],Operator1] = LinkedHashMap()
    private val registry2:Map[Tuple3[String,Class[_],Class[_]],Operator2] = LinkedHashMap()
    
    def register(name:String,c1:Class[_],op:Operator1) = {
        registry1.put((name,c1), op)
    }
    
    def register(name:String,c1:Class[_],c2:Class[_],op:Operator2) = {
        registry2.put((name,c1,c2), op)
    }
    
    def execute(name:String,v1:Value):Option[Value] = {
        registry1.get((name,v1.getClass)).map(op => op(v1))
    }
    
    def execute(name:String,v1:Value,v2:Value):Option[Value] = {
        registry2.get((name,v1.getClass,v2.getClass)).map(op => op(v1,v2))
    }
    
}
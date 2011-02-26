package org.encalmo.expression

/**
 * UnitOfValue trait
 */
trait UnitOfValue extends Expression {

	def expression:Expression = Void
	def multiplier:Double = 1
	
	override def eval():Expression = Number(multiplier)
}

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class BaseUnitOfValue (

		symbol:String,
		scale:Int = 0,
		dimension:Int = 1,
		system:UnitOfValueSystem
		
		
) extends UnitOfValue {
	
	override val expression:Expression = this
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	def dim(exp:Int):BaseUnitOfValue = copy(dimension=dimension*exp)
	def exp(sca:Int):BaseUnitOfValue = copy(scale=scale+sca)
	
	val name = system(scale).map(_.prefix).getOrElse("")+symbol
	
}

/**
 * UnitOfValueScale class
 */
case class UnitOfValueScale(

		prefix:String,
		multiplier:Double
){

}

/**
 * UnitOfValueSystem trait
 */
trait UnitOfValueSystem {

	def apply(scale:Int):Option[UnitOfValueScale] = None
	def multiplier(scale:Int):Double = this(scale).map(_.multiplier).getOrElse(throw new IllegalArgumentException("scale = "+scale))

}

/**
 * Empty system
 */
object EmptyUnitOfValueSystem extends UnitOfValueSystem

/**
 * Empty unit
 */
object EmptyUnitOfValue extends UnitOfValue
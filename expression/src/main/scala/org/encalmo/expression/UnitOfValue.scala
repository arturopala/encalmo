package org.encalmo.expression

/**
 * UnitOfValue trait
 */
trait UnitOfValue extends Expression {
	
    def name:UnitOfValueName = UnitOfValueName()
	def expression:Expression = Void
	def multiplier:Double = 1
	
	override def eval():Expression = Number(multiplier)
	
	/** Is non empty unit? */
	def isDefined:Boolean = false
	
	override def + (u:Expression):UnitOfValue = this
}

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class BaseUnitOfValue (

		baseName:UnitOfValueName,
		scale:Int = 0,
		dimension:Int = 1,
		system:UnitOfValueSystem
		
		
) extends UnitOfValue {
	
	override val expression:Expression = this
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	def dim(exp:Int):BaseUnitOfValue = copy(dimension=dimension*exp)
	def exp(sca:Int):BaseUnitOfValue = copy(scale=scale+sca)
	
	override val name:UnitOfValueName = baseName.withPrefix(system(scale).map(_.prefix).getOrElse(""))
	
	override def isDefined:Boolean = true
	
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
	def apply(name:String):Option[UnitOfValue] = None
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

/**
 * Unit name
 */
case class UnitOfValueName (
    baseName:String = "",
	prefix:Option[String] = None,
	suffix:Option[String] = None
){
    /** Adds prefix to unit name */
    def withPrefix(prefix:String):UnitOfValueName = UnitOfValueName(baseName,Some(prefix))
    
    /** Returns as String */
    override val toString:String = prefix.getOrElse("")+baseName+suffix.getOrElse("")
}



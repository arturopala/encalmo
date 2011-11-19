package org.encalmo.expression

/**
 * UnitOfValue trait
 */
trait UnitOfValue extends Expression {
	
    def characteristic:Characteristic = Characteristics.Unknown
    def name:UnitOfValueName = UnitOfValueName()
	def expression:Expression = Void
	def multiplier:Double = 1
	def dimension:Int = 1
	
	override def eval():Expression = Number(multiplier)
	
	/** Is this non empty unit? */
	def isDefined:Boolean = false
	
	/** Can we add this and that units? */
	def canAdd(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName && this.dimension == u.dimension
	
	override def + (e:Expression):UnitOfValue = e match {
        case u:UnitOfValue => {
            if(canAdd(u)) this else this
        }
        case _ => throw new IllegalStateException("Attempt to add unit and non-unit expression")
    }
}

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class BaseUnitOfValue (

		baseName:UnitOfValueName,
		scale:Int = 0,
		override val dimension:Int = 1,
		system:UnitOfValueSystem = SI,
		override val characteristic:Characteristic = Characteristics.Unknown
		
		
) extends UnitOfValue {
	
	override val expression:Expression = this
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	def dim(exp:Int):BaseUnitOfValue = copy(dimension=dimension*exp)
	def exp(sca:Int):BaseUnitOfValue = copy(scale=scale+sca)
	
	override val name:UnitOfValueName = baseName.withPrefix(system(scale).map(_.prefix).getOrElse(""))
	
	override def isDefined:Boolean = true
	
	/** Sets characteristic */
	def set(characteristic:Characteristic) = copy(characteristic = characteristic)
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

/**
 * Characteristic
 */
case class Characteristic (description:String)

/**
 * Characteristics dictionary
 */
object Characteristics {
    
    val Unknown:Characteristic = Characteristic("unknown")
    val Length:Characteristic = Characteristic("length")
    val Area:Characteristic = Characteristic("area")
    val Volume:Characteristic = Characteristic("volume")
    val Force:Characteristic = Characteristic("force")
    val Pressure:Characteristic = Characteristic("pressure")
    
}



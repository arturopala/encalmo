package org.encalmo.expression
import org.encalmo.common.{AdHocTraveler,Node}
import org.encalmo.common.Traveler

/**
 * UnitOfValue trait
 */
trait UnitOfValue extends Expression {
	
    def characteristic:Characteristic = Characteristics.Unknown
    def name:UnitOfValueName = EmptyUnitOfValueName
	def multiplier:Double = 1
	def dimension:Int = 1
	
	override def eval():Expression = Number(multiplier)
	
	def isDefined:Boolean = false
	
    def isSameBase(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName
    
    def isSameDimension(u:UnitOfValue):Boolean = this.dimension == u.dimension

	def isSame(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u)
	
	def isLargerThan(u:UnitOfValue):Boolean = multiplier > u.multiplier
	
	def baseUnit:UnitOfValue = this
	
	/** Sets dimension of unit */
	def dim(dim:Int):UnitOfValue
	
	def *(u:UnitOfValue) = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this.dim(this.dimension+u.dimension)
	    else ComplexUnitOfValue(Prod(this,u))
	}
	
	def /(u:UnitOfValue) = {
	    if(this==EmptyUnitOfValue) ComplexUnitOfValue(Quot(this,u))
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this.dim(this.dimension-u.dimension)
	    else ComplexUnitOfValue(Quot(this,u))
	}
	
	def +(u:UnitOfValue):UnitOfValue = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else IllegalUnitOfValue(this.name+"+"+u.name)
	}
	
	def -(u:UnitOfValue):UnitOfValue = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else IllegalUnitOfValue(this.name+"-"+u.name)
	}
	
	def toNameString:String = name.toString + (if(dimension == 1) "" else dimension)
    
}

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class SimpleUnitOfValue (

		baseName:UnitOfValueName = EmptyUnitOfValueName,
		scale:Int = 0,
		override val dimension:Int = 1,
		system:UnitOfValueSystem = EmptyUnitOfValueSystem,
		override val characteristic:Characteristic = Characteristics.Unknown
		
		
) extends UnitOfValue {
	
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	override def dim(newdim:Int):SimpleUnitOfValue = if(newdim!=0) copy(dimension=newdim) else EmptyUnitOfValue
	
	def exp(exp:Int):SimpleUnitOfValue = copy(scale=scale+exp)
	
	override val name:UnitOfValueName = baseName.withPrefix(system(scale).map(_.prefix).getOrElse(null))
	
	override def isDefined:Boolean = true
	
	/** Sets characteristic of unit */
	def set(characteristic:Characteristic) = copy(characteristic = characteristic)
	
	override lazy val baseUnit:UnitOfValue = system.find(baseName,0,dimension).getOrElse(this)
	
}

case class UnitOfValueNameBuilder extends Traveler[Expression] {
    
    val result = StringBuilder.newBuilder
    
    def toResult:String = result.toString
    
    override def onEnter(node:Node[Expression]):Unit = node.element match {
        case u:ComplexUnitOfValue => result.append("[")
        case EmptyUnitOfValue => result.append{
            if(node.parent!=null && node.parent.element.isInstanceOf[Quot] && node.position==0) "1"
            else "1"
        }
        case u:UnitOfValue => result.append(u.toNameString)
        case _ => Unit
    }
       
    override def onExit(node:Node[Expression]):Unit = node.element match {
        case u:ComplexUnitOfValue => {
            result.append("]")
            if(u.dimension!=1) result.append(u.dimension)
        }
        case _ => Unit
    }
    
    override def onBetweenChildren(node:Node[Expression], leftChild:Expression, rightChild:Expression):Unit = node.element match {
        case o:Operation2 => result.append(o.operator)
        case o:MultipleInfixOperation => result.append(o.operator)
        case _ => Unit
    }
    
}

case class ComplexUnitOfValue(
	expression:Expression,
	override val dimension:Int = 1
) extends UnitOfValue {
    
    private val nb = new UnitOfValueNameBuilder()
    expression.travel(traveler=nb)
    
    override val name:UnitOfValueName = UnitOfValueName(nb.toResult)
    
    override val children:Seq[Expression] = Seq(expression)
	
	override def isDefined:Boolean = true
	
	override def dim(newdim:Int):UnitOfValue = if(newdim!=0) copy(dimension=newdim) else EmptyUnitOfValue
	
	override def toNameString:String = name.toString
}

case class IllegalUnitOfValue(desc:String) extends UnitOfValue {
    
    override def dim(newdim:Int):IllegalUnitOfValue = this
    
    override def toNameString:String = "!"+desc+"!"
    
}

/** Empty unit */
object EmptyUnitOfValue extends SimpleUnitOfValue()

/**
 * UnitOfValueScale class
 */
case class UnitOfValueScale(

		prefix:String,
		multiplier:Double
)

/**
 * UnitOfValueSystem trait
 */
trait UnitOfValueSystem {

	def apply(scale:Int):Option[UnitOfValueScale] = Some(UnitOfValueScale("",Math.pow(10,scale)))
	def apply(name:String):Option[UnitOfValue] = unitMap.get(name)
	def find(baseName:UnitOfValueName,scale:Int,dimension:Int):Option[UnitOfValue] = Some(SimpleUnitOfValue(baseName,scale,dimension,this,Characteristics.Unknown))
	def multiplier(scale:Int):Double = this(scale).map(_.multiplier).getOrElse(Math.pow(10,scale))
	def unitSeq:Seq[UnitOfValue] = Seq()
	lazy val unitMap:Map[String,UnitOfValue] = unitSeq.map(u => (u.toNameString,u)).toMap

}

/**
 * Empty system
 */
object EmptyUnitOfValueSystem extends UnitOfValueSystem

/**
 * Empty name
 */
object EmptyUnitOfValueName extends UnitOfValueName()

/**
 * Unit name
 */
case class UnitOfValueName (
    baseName:String = "",
	prefix:Option[String] = None,
	suffix:Option[String] = None
){
    /** Adds prefix to unit name */
    def withPrefix(prefix:String):UnitOfValueName = UnitOfValueName(baseName,Option(prefix))
    
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
    val Weight:Characteristic = Characteristic("weight")
    
}



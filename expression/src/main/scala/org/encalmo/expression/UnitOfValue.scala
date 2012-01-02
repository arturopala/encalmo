package org.encalmo.expression
import org.encalmo.common.{AdHocTraveler,Node}
import org.encalmo.common.Traveler
import scala.collection.mutable.Stack
import scala.collection.mutable.Buffer
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

/**
 * UnitOfValue trait
 */
trait UnitOfValue extends Expression {
	
    def characteristic:Characteristic = Characteristics.None
    def name:UnitOfValueName = EmptyUnitOfValueName
	def multiplier:Double = 1
	def dimension:Double = 1
	def scale:Int = 0
	
	override def eval():Expression = Number(multiplier)
	
	def isDefined:Boolean = false
	
    def isSameBase(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName
    
    def isSameDimension(u:UnitOfValue):Boolean = this.dimension == u.dimension

	def isSame(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u)
	
	def isLargerThan(u:UnitOfValue):Boolean = multiplier > u.multiplier
	
	def baseUnit:UnitOfValue = this
	
	/** Sets dimension of unit */
	def dim(dim:Double):UnitOfValue
	/** Sets scale of unit */
	def exp(exp:Int):UnitOfValue
	
	def *(u:UnitOfValue) = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this.dim(this.dimension+u.dimension)
	    else ComplexUnitOfValue(Prod(this,u))
	}
	
	def /(u:UnitOfValue) = {
	    if(u == EmptyUnitOfValue) this 
	    else if(this==EmptyUnitOfValue) ComplexUnitOfValue(Quot(this,u))
	    else if(this.isSame(u)) this.dim(this.dimension-u.dimension)
	    else ComplexUnitOfValue(Quot(this,u))
	}
	
	def +(u:UnitOfValue):UnitOfValue = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else new IllegalUnitOfValue(this,"+",u)
	}
	
	def -(u:UnitOfValue):UnitOfValue = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else new IllegalUnitOfValue(this,"-",u)
	}
    
    def %(u:UnitOfValue) = {
        if(u == EmptyUnitOfValue) this 
        else if(this==EmptyUnitOfValue) this
        else if(this.isSame(u)) this
        else new IllegalUnitOfValue(this,"%",u)
    }
	
	def toNameString:String = name.toString + (if(dimension == 1) "" else UnitOfValue.dimensionFormat.format(dimension))
    
}

object UnitOfValue {
    val dimensionFormat = new DecimalFormat("0.#",new DecimalFormatSymbols(Locale.ENGLISH))
}

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class SimpleUnitOfValue (

		baseUnitName:UnitOfValueName = EmptyUnitOfValueName,
		override val scale:Int = 0,
		override val dimension:Double = 1,
		system:UnitOfValueSystem = EmptyUnitOfValueSystem,
		override val characteristic:Characteristic = Characteristics.None
		
		
) extends UnitOfValue {
	
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	override def dim(newdim:Double):UnitOfValue = if(newdim==0) EmptyUnitOfValue else system.find(baseUnitName.baseName,scale,newdim).getOrElse(copy(dimension = newdim))
	override def exp(exp:Int):UnitOfValue = system.find(baseUnitName.baseName,scale+exp,dimension).getOrElse(copy(scale=scale+exp))
	
	override val name:UnitOfValueName = baseUnitName.withPrefix(system(scale).map(_.prefix).getOrElse(null))
	
	override def isDefined:Boolean = true
	
	/** Sets characteristic of unit */
	def set(characteristic:Characteristic) = copy(characteristic = characteristic)
	
	override lazy val baseUnit:UnitOfValue = system.find(baseUnitName.baseName,0,dimension).getOrElse(this)
	
}

/** Empty unit */
object EmptyUnitOfValue extends SimpleUnitOfValue() {
    
    override def dim(newdim:Double):SimpleUnitOfValue = this
    override def exp(exp:Int):UnitOfValue = this
    override def toString:String = "EmptyUnitOfValue"
    
}

case class UnitOfValueNameBuilder extends Traveler[Expression] {
    
    private val stack = Stack[Buffer[String]]()
    
    stack.push(Buffer[String]())
    
    lazy val toResult:String = stack.top.mkString
    
    override def onEnter(node:Node[Expression]):Unit = node.element match {
        case u:ComplexUnitOfValue => {
            if(u.dimension!=1) stack.top.append("[")
        }
        case EmptyUnitOfValue => stack.top.append{
            if(node.parent!=null && node.parent.element.isInstanceOf[Quot] && node.position==0) "1"
            else "1"
        }
        case u:UnitOfValue => stack.top.append(u.toNameString)
        case o:MultipleInfixOperation => {
            stack.push(Buffer[String]())
        }
        case _ => Unit
    }
       
    override def onExit(node:Node[Expression]):Unit = node.element match {
        case u:ComplexUnitOfValue => {
            if(u.dimension!=1) stack.top.append("]"+UnitOfValue.dimensionFormat.format(u.dimension))
        }
        case o:MultipleInfixOperation => {
            val buff = stack.pop()
            stack.top.append(buff.sortBy(s => s.toLowerCase()).mkString(o.operator))
        }
        case _ => Unit
    }
    
    override def onBeforeChildEnter(node:Node[Expression], position:Int, child:Expression):Unit = {
        stack.push(Buffer[String]())
        node.element match {
            case o:Quot if position==1 && ((child.isInstanceOf[ComplexUnitOfValue] && child.asInstanceOf[ComplexUnitOfValue].dimension==1) || child.isInstanceOf[Operation]) => {
                stack.top.append("(");
            }
            case _ => Unit
        }
    }
    
    override def onAfterChildExit(node:Node[Expression], position:Int, child:Expression):Unit = {
        node.element match {
            case o:Quot if position==1 && ((child.isInstanceOf[ComplexUnitOfValue] && child.asInstanceOf[ComplexUnitOfValue].dimension==1) || child.isInstanceOf[Operation]) => {
                stack.top.append(")");
            }
            case _ => Unit
        }
        val buff = stack.pop()
        stack.top.append(buff.mkString)
    }
    
    override def onBetweenChildren(node:Node[Expression], leftChild:Expression, rightChild:Expression):Unit = node.element match {
        case o:Operation2 => stack.top.append(o.operator)
        case o:MultipleInfixOperation => Unit
        case _ => Unit
    }
    
}

case class ComplexUnitOfValue(
	expression:Expression,
	override val dimension:Double = 1,
	override val scale:Int = 0
) extends UnitOfValue {
    
    override lazy val name:UnitOfValueName = {
        val nb = new UnitOfValueNameBuilder()
        this.travel(traveler=nb)
        UnitOfValueName(nb.toResult)
    }
    
    override val children:Seq[Expression] = Seq(expression)
	
	override def isDefined:Boolean = true
	
	override def dim(newdim:Double):UnitOfValue = if(newdim==0) EmptyUnitOfValue else copy(dimension=newdim)
	
	override def exp(exp:Int):UnitOfValue = copy(scale=scale+exp)
	
	override def toNameString:String = name.toString
}

case class IllegalUnitOfValue(desc:String) extends UnitOfValue {
    
    def this(u1:UnitOfValue,operator:String,u2:UnitOfValue) = this(u1.toNameString+operator+u2.toNameString)
    
    override def dim(newdim:Double):IllegalUnitOfValue = IllegalUnitOfValue("("+desc+")^"+newdim)
    
    override def exp(exp:Int):UnitOfValue = IllegalUnitOfValue("("+desc+")exp"+exp)
    
    override def toNameString:String = "!"+desc+"!"
    
}

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
	def find(baseName:String,scale:Int,dimension:Double):Option[UnitOfValue] = Some(SimpleUnitOfValue(UnitOfValueName(baseName),scale,dimension,this,Characteristics.None))
	def multiplier(scale:Int):Double = this(scale).map(_.multiplier).getOrElse(Math.pow(10,scale))
	def units:Seq[UnitOfValue] = Seq()
	lazy val unitMap:Map[String,UnitOfValue] = units.map(u => (u.toNameString,u)).toMap

}

/**
 * Empty system
 */
object EmptyUnitOfValueSystem extends UnitOfValueSystem {
    override def toString:String = "Empty"
}

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
case class Characteristic (description:String) {
    override def toString:String = description
}

/**
 * Characteristics dictionary
 */
object Characteristics {
    
    val None:Characteristic = Characteristic("none")
    val Length:Characteristic = Characteristic("length")
    val Area:Characteristic = Characteristic("area")
    val Volume:Characteristic = Characteristic("volume")
    val Force:Characteristic = Characteristic("force")
    val Pressure:Characteristic = Characteristic("pressure")
    val Weight:Characteristic = Characteristic("weight")
    val Angle:Characteristic = Characteristic("angle")
    val Time:Characteristic = Characteristic("time")
    
}



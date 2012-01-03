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
    def isSameScale(u:UnitOfValue):Boolean = this.scale==u.scale
    def isSameDimension(u:UnitOfValue):Boolean = this.dimension == u.dimension
    def isSameBaseAndScale(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName && this.scale==u.scale
	def isSame(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u) && isSameScale(u)
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
    
    val dimensionFormat = Real.stringFormat
    
    def simplify(u:UnitOfValue):UnitOfValue = {
        u.mapAll(simplifyFx _) match {
            case u:UnitOfValue => u
            case e => ComplexUnitOfValue(e,1,0)
        }
    }
    
    def simplifyFx(e:Expression):Expression = e match {
        case Quot(l,r) if (ZERO==l) => ZERO
        case Quot(l,r) if (ONE==r) => l
        case EmptyUnitOfValue => ONE
        case ComplexUnitOfValue(exp:Expression,1,0) => exp
        case ComplexUnitOfValue(unit:UnitOfValue,dimension,scale) => dimension match {
            case 0 => EmptyUnitOfValue
            case 1 => unit
            case _ => unit.dim(dimension*unit.dimension).exp(scale+unit.scale)
        }
        case Prod(a,Quot(ONE,b)) => Quot(a,b)
        case Prod(Quot(ONE,a),b) => Quot(b,a)
        case Prod(ONE,a) => a
        case Prod(a,ONE) => a
        case Quot(n1:Number,n2:Number) => e.eval
        case Prod(n1:Number,n2:Number) => e.eval
        case Prod(Quot(n1:Number,a),n2:Number) => Quot((n1*n2).eval,a)
        case Prod(Quot(a,n1:Number),n2:Number) => Prod(a,(n2/n1).eval)
        case Prod(n1:Number,Quot(n2:Number,a)) => Quot((n1*n2).eval,a)
        case Prod(n1:Number,Quot(a,n2:Number)) => Prod(a,(n1/n2).eval)
        case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyQuot(u1,u2)
        case Prod(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyProd(u1,u2)
        case Quot(Prod(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod(a,simplifyQuot(u1,u2))
        case Quot(Prod(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod(a,simplifyQuot(u1,u2))
        case Quot(u1:SimpleUnitOfValue,Prod(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)
        case Quot(u1:SimpleUnitOfValue,Prod(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)
        case Prod(Quot(u1:SimpleUnitOfValue,a),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod(Quot(b,a),simplifyQuot(u1,u2))
        case Prod(Quot(a,u1:SimpleUnitOfValue),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod(Quot(a,b),simplifyQuot(u2,u1))
        case Prod(Quot(u1:SimpleUnitOfValue,a),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Quot(simplifyProd(u1,u2),Prod(a,b))
        case Prod(Quot(a,u1:SimpleUnitOfValue),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(Prod(a,b),simplifyProd(u1,u2))
        case Quot(Prod(a,u1:SimpleUnitOfValue),Prod(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod(u1:SimpleUnitOfValue,a),Prod(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod(u1:SimpleUnitOfValue,a),Prod(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod(a,u1:SimpleUnitOfValue),Prod(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod(Quot(a,b),simplifyQuot(u1,u2))
        case Prod(Quot(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(simplifyProd(u1,u2),a)
        case Prod(u1:SimpleUnitOfValue,Quot(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyProd(u1,u2),a)
        case Prod(Quot(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod(a,simplifyQuot(u2,u1))
        case Prod(u1:SimpleUnitOfValue,Quot(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod(a,simplifyQuot(u1,u2))
        case _ => e
    }
    
    def simplifyQuot(u1:UnitOfValue,u2:UnitOfValue):Expression = {
        if(u1.isSameScale(u2)) {
            if(u1.dimension>=u2.dimension) u1.dim(u1.dimension-u2.dimension)
            else Quot(1,u1.dim(u2.dimension-u1.dimension))
        }
        else {
            u1.multiplier/u2.multiplier match {
                case 1 => u1.baseUnit.dim(u1.dimension-u2.dimension)
                case m if m>1 && u1.dimension>=u2.dimension => Prod(Number(Real(m)),u1.baseUnit.dim(u1.dimension-u2.dimension))
                case m if m<1 && u1.dimension>=u2.dimension => Quot(u1.baseUnit.dim(u1.dimension-u2.dimension),Number(Real(m).inverse))
                case m if m>1 && u1.dimension<u2.dimension => Quot(Number(Real(m)),u1.baseUnit.dim(u2.dimension-u1.dimension))
                case m if m<1 && u1.dimension<u2.dimension => Prod(Number(Real(m).inverse),u1.baseUnit.dim(u2.dimension-u1.dimension))
            }
        }
    }
    
    def simplifyProd(u1:UnitOfValue,u2:UnitOfValue):Expression = {
        if(u1.isSameScale(u2)) u1.dim(u1.dimension+u2.dimension)
        else {
            u1.multiplier*u2.multiplier match {
                case 1 => u1.baseUnit.dim(u1.dimension-u2.dimension)
                case m if m>1 => Prod(Number(Real(m)),u1.baseUnit.dim(u1.dimension+u2.dimension))
                case m if m<1 => Quot(u1.baseUnit.dim(u1.dimension+u2.dimension),Number(Real(m).inverse))
            }
        }
    }
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
        case Number(r,u) => stack.top.append(r.toString())
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

    final override def map(f: Transformation): Expression = {
        val ve = expression.map(f);
        f(if(ve == expression) this else copy(expression = ve))
    }
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
    val MomentOfForce:Characteristic = Characteristic("momentOfForce")
    val Pressure:Characteristic = Characteristic("pressure")
    val Weight:Characteristic = Characteristic("weight")
    val Angle:Characteristic = Characteristic("angle")
    val Time:Characteristic = Characteristic("time")
    
}



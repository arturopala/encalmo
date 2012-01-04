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
	
	def isDefined:Boolean = false
	def isBaseUnit = scale==0
    def isSameBase(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName
    def isSameScale(u:UnitOfValue):Boolean = this.scale==u.scale
    def isSameDimension(u:UnitOfValue):Boolean = this.dimension == u.dimension
    def isSameBaseAndScale(u:UnitOfValue):Boolean = this.name.baseName==u.name.baseName && this.scale==u.scale
    def isSameBaseAndDimension(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u)
	def isSame(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u) && isSameScale(u)
	def isLargerThan(u:UnitOfValue):Boolean = multiplier > u.multiplier
	def isLargerScaleThan(u:UnitOfValue):Boolean = scale > u.scale
	
	/** Base unit of this unit's family */
	def baseUnit:UnitOfValue = this
    /** Simplified unit variant */
    def simplifiedUnit:UnitOfValue
    /** Expanded unit variant */
    def expandedUnit:UnitOfValue
	
	/** Sets dimension of unit */
	def dim(dim:Double):UnitOfValue
	/** Sets scale of unit */
	def exp(exp:Int):UnitOfValue
	
	def *(u:UnitOfValue):UnitOfValue = {
	    if(this==EmptyUnitOfValue) u 
	    else if(u == EmptyUnitOfValue) this 
	    else if(this.isSameBaseAndScale(u)) this.dim(this.dimension+u.dimension)
	    else ComplexUnitOfValue(Prod(this,u))
	}
	
	def *(n:Number):UnitOfValue = n match {
	    case ONE => this
	    case _=> ComplexUnitOfValue(Prod(this,n))
	}
	
	def /(u:UnitOfValue):UnitOfValue = {
	    if(u == EmptyUnitOfValue) this 
	    else if(this==EmptyUnitOfValue) ComplexUnitOfValue(Quot(this,u))
	    else if(this==u) EmptyUnitOfValue
	    else ComplexUnitOfValue(Quot(this,u))
	}

    def /(n:Number):UnitOfValue = n match {
        case ONE => this
        case _=> ComplexUnitOfValue(Quot(this,n))
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
    
    def %(u:UnitOfValue):UnitOfValue = {
        if(u == EmptyUnitOfValue) this 
        else if(this==EmptyUnitOfValue) this
        else if(this.isSame(u)) this
        else new IllegalUnitOfValue(this,"%",u)
    }
	
	def toNameString:String = name.toString + (if(dimension == 1) "" else UnitOfValue.dimensionFormat.format(dimension))
    
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
	override lazy val simplifiedUnit:UnitOfValue = this
	override lazy val expandedUnit:UnitOfValue = system.expand(this)
	
}

/** Empty unit */
object EmptyUnitOfValue extends SimpleUnitOfValue() {
    
    override def dim(newdim:Double):SimpleUnitOfValue = this
    override def exp(exp:Int):UnitOfValue = this
    override def toString:String = "EmptyUnitOfValue"
    override lazy val simplifiedUnit:UnitOfValue = this
    override lazy val expandedUnit:UnitOfValue = this
    
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
        case o:Prod2 => {
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
        case o:Prod2 => {
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
        case o:Prod2 => Unit
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
    
    override def eval():Expression = copy(expression=expression.eval)
	override def isDefined:Boolean = true
	override def dim(newdim:Double):UnitOfValue = if(newdim==0) EmptyUnitOfValue else copy(dimension=newdim)
	override def exp(exp:Int):UnitOfValue = copy(scale=scale+exp)
	override def toNameString:String = name.toString
	override lazy val simplifiedUnit:UnitOfValue = UnitOfValue.simplify(this)
	override lazy val expandedUnit:UnitOfValue = UnitOfValue.expand(this)

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
    override lazy val simplifiedUnit:UnitOfValue = this
    override lazy val expandedUnit:UnitOfValue = this
    
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
	def expand(u:UnitOfValue):UnitOfValue = u
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

/**
 * UnitOfValue factory
 */
object UnitOfValue {
    
    val dimensionFormat = Real.stringFormat
    
    def expand(u:UnitOfValue):UnitOfValue = {
        val e = u.mapAll(expandFx _) match {
            case u:UnitOfValue => u
            case e => ComplexUnitOfValue(e)
        }
        val s = simplify(e)
        s
    }
    
    private def expandFx(e:Expression):Expression = e match {
        case u:SimpleUnitOfValue => u.expandedUnit
        case _ => e
    }
    
    def simplify(u:UnitOfValue):UnitOfValue = {
        u.mapAll(simplifyFx _) match {
            case u:UnitOfValue => u
            case e => ComplexUnitOfValue(e)
        }
    }
    
    /** Unit expression simplification rules */
    private def simplifyFx(e:Expression):Expression = e match {
        
        // rules for operation simplification
        
        case p:Prod => p.eval match { case p2:Prod => p2.toProd2; case x => x }
        case Quot(a,b) if a==b => ONE                // a/a = 1
        case Quot(Quot(a,b),c) => Quot(a,Prod2(b,c)) // (a/b)/c = a/[b*c]
        case Quot(a,Quot(b,c)) => Quot(Prod2(a,c),b) // a/(b/c) = [a*c]/b
        
        // rules for values simplification
        
        case Quot(n1:Value,n2:Value) => e.eval  // evaluation
        case Prod2(n1:Value,n2:Value) => e.eval  // evaluation
        case Prod2(ONE,a) => a                   //  1 * a = a
        case Prod2(a,ONE) => a                   //  a * 1 = a
        case Prod2(ZERO,a) => ZERO               //  0 * a = 0
        case Prod2(a,ZERO) => ZERO               //  a * 0 = 0
        case Quot(ZERO,a) => ZERO               //  0/a = 0
        case Quot(a,ONE)  => a                  //  a/1 = a
        case Prod2(a,Quot(ONE,b)) => Quot(a,b)   //  a * 1/b = a/b
        case Prod2(Quot(ONE,a),b) => Quot(b,a)   //  1/a * b = b/a
        
        case Prod2(Quot(n1:Value,a),n2:Value) => Quot((n1*n2).eval,a)  //   n1/a * n2 = [n1*n2]/a
        case Prod2(Quot(a,n1:Value),n2:Value) => Prod2(a,(n2/n1).eval) //   a/n1 * n2 = a*[n2/n1]
        case Prod2(n1:Value,Quot(n2:Value,a)) => Quot((n1*n2).eval,a)  //   n1 * n2/a = [n1*n2]/a
        case Prod2(n1:Value,Quot(a,n2:Value)) => Prod2(a,(n1/n2).eval) //   n1 * a/n2 = a*[n1/n2]
        
        case Prod2(Prod2(n1:Value,a),n2:Value) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        case Prod2(Prod2(a,n1:Value),n2:Value) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        case Prod2(n1:Value,Prod2(n2:Value,a)) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        case Prod2(n1:Value,Prod2(a,n2:Value)) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        
        //case Quot(Quot(n1:Value,a),n2:Value) => Quot((n1/n2).eval,a) //   n1/a / n2 = [n1/n2]/a
        //case Quot(Quot(a,n1:Value),n2:Value) => Quot(a,(n1*n2).eval) //   a/n1 / n2 = a/[n1*n2]
        //case Quot(n1:Value,Quot(a,n2:Value)) => Quot((n1*n2).eval,a) //   n1 / a/n2 = [n1*n2]/a
        //case Quot(n1:Value,Quot(n2:Value,a)) => Prod2(a,(n1/n2).eval)//   n1 / n2/a = [n1/n2]*a
        
        case Prod2(Quot(n1:Value,a),Quot(n2:Value,b)) => Quot((n1*n2).eval,Prod2(a,b)) //   n1/a * n2/b = [n1*n2]/[a*b]
        case Prod2(Quot(n1:Value,a),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(b,a)) //   n1/a * b/n2 = [n1/n2]*[b/a]
        case Prod2(Quot(a,n1:Value),Quot(n2:Value,b)) => Prod2((n2/n1).eval,Quot(a,b)) //   a/n1 * n2/b = [n2/n1]*[a/b]
        case Prod2(Quot(a,n1:Value),Quot(b,n2:Value)) => Quot(Prod2(a,b),(n1/n2).eval) //   a/n1 * b/n2 = [a*b]/[n1*n2]
        
        case Quot(Prod2(n1:Value,a),Prod2(n2:Value,b)) => Prod2((n1/n2).eval,Quot(a,b)) //   n1*a / n2*b = [n1/n2]*[a/b]
        case Quot(Prod2(n1:Value,a),Prod2(b,n2:Value)) => Prod2((n1/n2).eval,Quot(a,b)) //   n1*a / b*n2 = [n1/n2]*[a/b]
        case Quot(Prod2(a,n1:Value),Prod2(n2:Value,b)) => Prod2((n1/n2).eval,Quot(a,b)) //   a*n1 / n2*b = [n1/n2]*[a/b]
        case Quot(Prod2(a,n1:Value),Prod2(b,n2:Value)) => Prod2((n1/n2).eval,Quot(a,b)) //   a*n1 / b*n2 = [n1/n2]*[a/b]
        
        //case Quot(Prod2(n1:Value,a),Quot(n2:Value,b)) => Prod2((n1/n2).eval,Prod2(a,b)) //   n1*a / n2/b = n1*a * b/n2 = [n1/n2]*[a*b]
        //case Quot(Prod2(n1:Value,a),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(a,b)) //   n1*a / b/n2 = n1*a * n2/b = [n1*n2]*[a/b]
        //case Quot(Prod2(a,n1:Value),Quot(n2:Value,b)) => Prod2((n1/n2).eval,Prod2(a,b)) //   a*n1 / n2/b = n1*a * b/n2 = [n1/n2]*[a*b]
        //case Quot(Prod2(a,n1:Value),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(a,b)) //   a*n1 / b/n2 = n1*a * n2/b = [n1*n2]*[a/b]
        
        //case Quot(Quot(n1:Value,a),Prod2(n2:Value,b)) => Quot((n1/n2).eval,Prod2(a,b)) //   n1/a / n2*b = n1 / n2*a*b = [n1/n2]/[a*b]
        //case Quot(Quot(n1:Value,a),Prod2(b,n2:Value)) => Quot((n1/n2).eval,Prod2(a,b)) //   n1/a / b*n2 = n1 / n2*a*b = [n1/n2]/[a*b]
        //case Quot(Quot(a,n1:Value),Prod2(n2:Value,b)) => Quot(Quot(a,b),(n1*n2).eval) //   a/n1 / n2*b = a / n2*n1*b = [a/b]/[n1*n2]
        //case Quot(Quot(a,n1:Value),Prod2(b,n2:Value)) => Quot(Quot(a,b),(n1*n2).eval) //   a/n1 / b*n2 = a / n2*n1*b = [a/b]/[n1*n2]
        
        // rules for units simplification
        
        case EmptyUnitOfValue => ONE
        case ComplexUnitOfValue(expression,1,0) => expression
        case ComplexUnitOfValue(unit:UnitOfValue,dimension,scale) => dimension match {
            case 0 => EmptyUnitOfValue
            case 1 => unit
            case _ => unit.dim(dimension*unit.dimension).exp(scale+unit.scale)
        }
        case ComplexUnitOfValue(expression,dimension,scale) if dimension>1 => Prod((for (a <- 1 to dimension.toInt) yield expression):_*)
        
        case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyQuot(u1,u2)
        case Prod2(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyProd2(u1,u2)
        
        case Quot(Prod2(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2))
        case Quot(Prod2(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2))
        case Quot(u1:SimpleUnitOfValue,Prod2(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)
        case Quot(u1:SimpleUnitOfValue,Prod2(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)
        
        case Prod2(Prod2(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a) //   n1*a * n2 = [n1*n2]*a
        case Prod2(Prod2(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a) //   n1*a * n2 = [n1*n2]*a
        case Prod2(u1:SimpleUnitOfValue,Prod2(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a) //   n1*a * n2 = [n1*n2]*a
        case Prod2(u1:SimpleUnitOfValue,Prod2(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a) //   n1*a * n2 = [n1*n2]*a
        
        case Prod2(Quot(u1:SimpleUnitOfValue,a),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyQuot(u1,u2))
        case Prod2(Quot(a,u1:SimpleUnitOfValue),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))
        case Prod2(Quot(u1:SimpleUnitOfValue,a),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),Prod2(a,b))
        case Prod2(Quot(a,u1:SimpleUnitOfValue),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(Prod2(a,b),simplifyProd2(u1,u2))
        
        case Quot(Prod2(a,u1:SimpleUnitOfValue),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod2(u1:SimpleUnitOfValue,a),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod2(u1:SimpleUnitOfValue,a),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2))
        case Quot(Prod2(a,u1:SimpleUnitOfValue),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2))
        
        case Prod2(Quot(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a)
        case Prod2(u1:SimpleUnitOfValue,Quot(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a)
        case Prod2(Quot(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u2,u1))
        case Prod2(u1:SimpleUnitOfValue,Quot(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2))
        
        //case Quot(Quot(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(a,simplifyProd2(u1,u2))
        //case Quot(Quot(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)
        //case Quot(u1:SimpleUnitOfValue,Quot(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a)
        //case Quot(u1:SimpleUnitOfValue,Quot(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Prod2(simplifyQuot(u1,u2),a)
        
        case Prod2(Quot(a,Prod2(b,u1:SimpleUnitOfValue)),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))
        case Prod2(Quot(a,Prod2(u1:SimpleUnitOfValue,b)),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))
        case Prod2(Quot(Prod2(b,u1:SimpleUnitOfValue),a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u2,u1))
        case Prod2(Quot(Prod2(u1:SimpleUnitOfValue,b),a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u2,u1))
        
        case Prod2(u2:SimpleUnitOfValue,Quot(a,Prod2(b,u1:SimpleUnitOfValue))) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))
        case Prod2(u2:SimpleUnitOfValue,Quot(a,Prod2(u1:SimpleUnitOfValue,b))) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))
        case Prod2(u2:SimpleUnitOfValue,Quot(Prod2(b,u1:SimpleUnitOfValue),a)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u2,u1))
        case Prod2(u2:SimpleUnitOfValue,Quot(Prod2(u1:SimpleUnitOfValue,b),a)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u2,u1))
        
        case Prod2(u1:SimpleUnitOfValue,Prod2(Quot(u2:SimpleUnitOfValue,a),b)) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),Quot(b,a))
        case Prod2(u1:SimpleUnitOfValue,Prod2(a,Quot(u2:SimpleUnitOfValue,b))) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),Quot(a,b))
        case Prod2(Prod2(Quot(u2:SimpleUnitOfValue,a),b),u1:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),Quot(b,a))
        case Prod2(Prod2(a,Quot(u2:SimpleUnitOfValue,b)),u1:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),Quot(a,b))
        
        case Quot(u1:SimpleUnitOfValue,Prod2(a,Quot(b,u2:SimpleUnitOfValue))) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),Prod2(a,b))
        
        case _ => e
    }
    
    private def simplifyQuot(u1:UnitOfValue,u2:UnitOfValue):Expression = {
        if(u1.isSameScale(u2)) {
            if(u1.dimension>=u2.dimension) u1.dim(u1.dimension-u2.dimension)
            else Quot(ONE,u1.dim(u2.dimension-u1.dimension))
        } 
        else {
            u1.multiplier/u2.multiplier match {
                case 1 if u1.dimension>=u2.dimension => u1.baseUnit.dim(u1.dimension-u2.dimension)
                case 1 if u1.dimension<u2.dimension => Quot(ONE,u1.dim(u2.dimension-u1.dimension))
                case m if m>1 && u1.dimension>=u2.dimension => Prod(Number(Real(m)),u1.baseUnit.dim(u1.dimension-u2.dimension))
                case m if m<1 && u1.dimension>=u2.dimension => Quot(u1.baseUnit.dim(u1.dimension-u2.dimension),Number(Real(m).inverse))
                case m if m>1 && u1.dimension<u2.dimension => Quot(Number(Real(m)),u1.baseUnit.dim(u2.dimension-u1.dimension))
                case m if m<1 && u1.dimension<u2.dimension => Prod(Number(Real(m).inverse),u1.baseUnit.dim(u2.dimension-u1.dimension))
            }
        }
    }
    
    private def simplifyProd2(u1:UnitOfValue,u2:UnitOfValue):Expression = {
        if(u1.isSameScale(u2)) u1.dim(u1.dimension+u2.dimension)
        else {
            u1.multiplier*u2.multiplier match {
                case 1 => u1.baseUnit.dim(u1.dimension-u2.dimension)
                case m if m>1 => Prod2(Number(Real(m)),u1.baseUnit.dim(u1.dimension+u2.dimension))
                case m if m<1 => Quot(u1.baseUnit.dim(u1.dimension+u2.dimension),Number(Real(m).inverse))
            }
        }
    }
}



package org.encalmo.expression
import org.encalmo.common.Node
import org.encalmo.common.TreeVisitor
import scala.collection.mutable

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
    def isSameBase(u:UnitOfValue):Boolean = this.name.baseName eq u.name.baseName
    def isSameScale(u:UnitOfValue):Boolean = this.scale==u.scale
    def isSameDimension(u:UnitOfValue):Boolean = this.dimension == u.dimension
    def isSameBaseAndScale(u:UnitOfValue):Boolean = (this.name.baseName eq u.name.baseName) && this.scale==u.scale
    def isSameBaseAndDimension(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u)
	def isSame(u:UnitOfValue):Boolean = isSameBase(u) && isSameDimension(u) && isSameScale(u)
	def isLargerThan(u:UnitOfValue):Boolean = if(this.expandedUnitMultiplier != u.expandedUnitMultiplier) this.expandedUnitMultiplier > u.expandedUnitMultiplier 
	    else this.toNameString.size < u.toNameString.size
	def isLargerScaleThan(u:UnitOfValue):Boolean = scale > u.scale
	
	def isSameExpandedUnit(u:UnitOfValue):Boolean = this.expandedUnitBase.toNameString==u.expandedUnitBase.toNameString
	def isSameExpandedUnitAndMultiplier(u:UnitOfValue):Boolean = this.expandedUnit==u.expandedUnit
	
	/** Base unit of this unit's family */
	def baseUnit:UnitOfValue = this
    /** Simplified unit variant */
    def simplifiedUnit:(UnitOfValue,Double)
    /** Expanded unit variant */
    def expandedUnit:(UnitOfValue,Double)
    
    def expandedUnitBase:UnitOfValue = expandedUnit._1
    def expandedUnitMultiplier:Double = expandedUnit._2
    
    def simplifiedUnitBase:UnitOfValue = simplifiedUnit._1
    def simplifiedUnitMultiplier:Double = simplifiedUnit._2
	
	/** Sets dimension of unit */
	def dim(dim:Double):UnitOfValue
	/** Sets scale of unit */
	def exp(exp:Int):UnitOfValue
	
	def *(u:UnitOfValue):UnitOfValue = {
	    if(this eq EmptyUnitOfValue) u 
	    else if(u eq EmptyUnitOfValue) this 
	    else if(this.isSameBaseAndScale(u)) this.dim(this.dimension+u.dimension)
	    else ComplexUnitOfValue(Prod(this,u))
	}
	
	def *(n:Number):UnitOfValue = n match {
	    case ONE => this
	    case _=> ComplexUnitOfValue(Prod(this,n))
	}
	
	def /(u:UnitOfValue):UnitOfValue = {
	    if(u eq EmptyUnitOfValue) this 
	    else if(this eq EmptyUnitOfValue) ComplexUnitOfValue(Quot(this,u))
	    else if(this==u) EmptyUnitOfValue
	    else ComplexUnitOfValue(Quot(this,u))
	}

    def /(n:Number):UnitOfValue = n match {
        case ONE => this
        case _=> ComplexUnitOfValue(Quot(this,n))
    }
	
	def +(u:UnitOfValue):UnitOfValue = {
	    if(this eq EmptyUnitOfValue) u 
	    else if(u eq EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else throw new IllegalUnitOperationException(this,"+",u)
	}
	
	def -(u:UnitOfValue):UnitOfValue = {
	    if(this eq EmptyUnitOfValue) u 
	    else if(u eq EmptyUnitOfValue) this 
	    else if(this.isSame(u)) this 
	    else throw new IllegalUnitOperationException(this,"-",u)
	}
    
    def %(u:UnitOfValue):UnitOfValue = {
        if(u eq EmptyUnitOfValue) this 
        else if(this eq EmptyUnitOfValue) this
        else if(this.isSame(u)) this
        else throw new IllegalUnitOperationException(this,"%",u)
    }

    def divideDimension(n: Double): UnitOfValue
    def multiplyDimension(n: Double): UnitOfValue

	def toNameString:String = name.toString + (if(dimension == 1) "" else UnitOfValue.dimensionFormat.format(dimension))
    
}

class IllegalUnitOperationException(u1: UnitOfValue, op: String, u2: UnitOfValue) extends RuntimeException(s"${u1.toNameString} $op ${u2.toNameString}")

/**
 * BaseUnitOfValue class
 * @author artur.opala
 */
case class SimpleUnitOfValue (

		baseUnitName:UnitOfValueName = EmptyUnitOfValueName,
		override val scale:Int = 0,
		override val dimension:Double = 1,
		system:UnitOfValueSystem = EmptyUnitOfValueSystem,
		override val characteristic:Characteristic = Characteristics.None,
		customName:Option[UnitOfValueName] = None
		
		
) extends UnitOfValue {
	
	override val multiplier:Double = Math.pow(system.multiplier(scale),dimension)
	
	override def dim(newdim:Double):UnitOfValue = if(newdim==0) EmptyUnitOfValue else system.find(baseUnitName.baseName,scale,newdim).getOrElse(copy(dimension = newdim))
	override def exp(exp:Int):UnitOfValue = system.find(baseUnitName.baseName,scale+exp,dimension).getOrElse(copy(scale=scale+exp))
	
	override val name:UnitOfValueName = customName.getOrElse(baseUnitName.withPrefix(system(scale).map(_.prefix).getOrElse(null)))
	
	override def isDefined:Boolean = true
	
	/** Sets characteristic of unit */
	def set(characteristic:Characteristic) = copy(characteristic = characteristic)
	
	override lazy val baseUnit:UnitOfValue = system.find(baseUnitName.baseName,0,dimension).getOrElse(SimpleUnitOfValue(baseUnitName,0,dimension,system,characteristic))
	override lazy val simplifiedUnit:(UnitOfValue,Double) = if(baseUnitName eq EmptyUnitOfValueName) (baseUnit,multiplier) else (this,1)
	override lazy val expandedUnit:(UnitOfValue,Double) = UnitOfValue.expandUnit(this)

    override lazy val toString:String = toNameString

    override def divideDimension(n: Double): UnitOfValue = if(n>0) system.find(baseUnitName.baseName,scale,dimension/n).getOrElse(copy(dimension = dimension/n)) else throw new IllegalStateException("Illegal value of unit dimension divider: "+n)
    override def multiplyDimension(n: Double): UnitOfValue = if(n>0) system.find(baseUnitName.baseName,scale,dimension*n).getOrElse(copy(dimension = dimension*n)) else throw new IllegalStateException("Illegal value of unit dimension multiplier: "+n)
	
    lazy val one: Number = Number(1,this)

}

/** Empty unit */
object EmptyUnitOfValue extends SimpleUnitOfValue() {
    
    override def dim(newdim:Double):SimpleUnitOfValue = this
    override def exp(exp:Int):UnitOfValue = this
    override lazy val toString:String = "_"
    override lazy val simplifiedUnit:(UnitOfValue,Double) = (this,1)
    override lazy val expandedUnit:(UnitOfValue,Double) = (this,1)
    override def divideDimension(n: Double): UnitOfValue = this
    override def multiplyDimension(n: Double): UnitOfValue = this

}

case class UnitOfValueNameBuilder() extends TreeVisitor[Expression] {
    
    private val stack = mutable.Stack[mutable.Buffer[String]]()
    
    stack.push(mutable.Buffer[String]())
    
    lazy val toResult:String = stack.top.mkString
    
    override def onEnter(node:Node[Expression]):Unit = node.element match {
        case u:ComplexUnitOfValue => {
            if(u.dimension!=1) stack.top.append("[")
        }
        case u if u eq EmptyUnitOfValue => stack.top.append{
            if(node.parent!=null && node.parent.element.isInstanceOf[Quot] && node.position==0) "1"
            else "1"
        }
        case u:UnitOfValue => stack.top.append(u.toNameString)
        case o:MultipleInfixOperation => {
            stack.push(mutable.Buffer[String]())
        }
        case o:Prod2 => {
            stack.push(mutable.Buffer[String]())
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
            stack.top.append(buff.sortBy(s => s.toLowerCase).mkString(o.operator))
        }
        case o:Prod2 => {
            val buff = stack.pop()
            stack.top.append(buff.sortBy(s => s.toLowerCase).mkString(o.operator))
        }
        case _ => Unit
    }
    
    override def onBeforeChildEnter(node:Node[Expression], position:Int, child:Expression):Unit = {
        stack.push(mutable.Buffer[String]())
        node.element match {
            case o:Quot if position==1 && ((child.isInstanceOf[ComplexUnitOfValue] && child.asInstanceOf[ComplexUnitOfValue].dimension==1) || child.isInstanceOf[Operation]) => {
                stack.top.append("(")
            }
            case _ => Unit
        }
    }
    
    override def onAfterChildExit(node:Node[Expression], position:Int, child:Expression):Unit = {
        node.element match {
            case o:Quot if position==1 && ((child.isInstanceOf[ComplexUnitOfValue] && child.asInstanceOf[ComplexUnitOfValue].dimension==1) || child.isInstanceOf[Operation]) => {
                stack.top.append(")")
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
        this.visit(nb)
        UnitOfValueName(nb.toResult)
    }
    
    override val children:Seq[Expression] = Seq(expression)
    
    override def eval():Expression = copy(expression=expression.eval())
	override def isDefined:Boolean = true
	override def dim(newdim:Double):UnitOfValue = if(newdim==0) EmptyUnitOfValue else copy(dimension=newdim)
	override def exp(exp:Int):UnitOfValue = copy(scale=scale+exp)
	override def toNameString:String = name.toString

	override lazy val simplifiedUnit:(UnitOfValue,Double) = UnitOfValue.simplifyUnit(this)
	override lazy val expandedUnit:(UnitOfValue,Double) = UnitOfValue.expandUnit(this)

    final override def map(f: Transformation): Expression = {
        val ve = expression.map(f)
        f(if(ve == expression) this else copy(expression = ve))
    }
    
    override lazy val toString:String = toNameString

    override def divideDimension(n: Double): UnitOfValue = if(n>0) expression match {
        case Quot(l: UnitOfValue,r: UnitOfValue) => copy(expression=Quot(l.divideDimension(n),r.divideDimension(n)))
        case Quot(l,r: UnitOfValue) => copy(expression=Quot(l,r.divideDimension(n)))
        case Quot(l: UnitOfValue,r) => copy(expression=Quot(l.divideDimension(n),r))
        case _ => copy(dimension/n)
    } else throw new IllegalStateException("Illegal value of unit dimension divider: "+n)

    override def multiplyDimension(n: Double): UnitOfValue = if(n>0) expression match {
        case Quot(l: UnitOfValue,r: UnitOfValue) => copy(expression=Quot(l.multiplyDimension(n),r.multiplyDimension(n)))
        case Quot(l,r: UnitOfValue) => copy(expression=Quot(l,r.multiplyDimension(n)))
        case Quot(l: UnitOfValue,r) => copy(expression=Quot(l.multiplyDimension(n),r))
        case _ => copy(dimension*n)
    } else throw new IllegalStateException("Illegal value of unit dimension multiplier: "+n)

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
    
    def expandUnit(u:UnitOfValue):(UnitOfValue,Double) = {
        val e1:Expression = u.mapAll(simplifyFx)
        val e2 = e1.mapAll(expandFx)
        val e3 = e2.mapAll(simplifyFx)
        val n:Number = e3.mapAll(multiplierFx).eval() match {
            case n:Number => n
            case _ => ZERO
        }
        val u2:UnitOfValue = encapsulateUnitFx(e3.mapAll(cleanNumbersFx).mapAll(simplifyFx).eval())
        (u2,n.r.d)
    }
    
    def evaluateExpandedUnitMultiplier(u:UnitOfValue):Expression = {
        u.mapAll(multiplierFx).eval()
    }
    
    def simplifyUnit(u:UnitOfValue):(UnitOfValue,Double) = {
        val e1 = u.mapAll(simplifyFx)
        val n:Number = e1.mapAll(numberFx).eval() match {
            case n:Number => n
            case _ => ZERO
        }
        val u2:UnitOfValue = encapsulateUnitFx(e1.mapAll(cleanNumbersFx).mapAll(simplifyFx) match {
            case ONE => EmptyUnitOfValue
            case x => x
        })
        (u2,n.r.d)
    }
    
    def encapsulateUnitFx:(Expression)=>UnitOfValue = {
        case u:UnitOfValue => u
        case e => ComplexUnitOfValue(e)
    }
    
    private def expandFx:(Expression)=>Expression = {
        case u:SimpleUnitOfValue => {
            u.multiplier match {
                case 1 => u.system.expand(u)
                case m => Prod2(Number(m),u.system.expand(u.baseUnit))
            }
            
        }
        case e => e
    }
    
    private def numberFx:(Expression)=>Expression = {
        case u if u eq EmptyUnitOfValue => ONE
        case su:SimpleUnitOfValue => ONE
        case ComplexUnitOfValue(expression,1,0) => expression
        case ComplexUnitOfValue(unit:UnitOfValue,dimension,scale) => dimension match {
            case 0 => EmptyUnitOfValue
            case 1 => unit
            case _ => unit.dim(dimension*unit.dimension).exp(scale+unit.scale)
        }
        case ComplexUnitOfValue(expression,dimension,scale) if dimension>1 => Prod((for (a <- 1 to dimension.toInt) yield expression):_*)
        case e => e
    }
    
    private def multiplierFx:(Expression)=>Expression = {
        case u if u eq EmptyUnitOfValue => ONE
        case su:SimpleUnitOfValue => Number(su.multiplier)
        case ComplexUnitOfValue(expression,1,0) => expression
        case ComplexUnitOfValue(unit:UnitOfValue,dimension,scale) => dimension match {
            case 0 => EmptyUnitOfValue
            case 1 => unit
            case _ => unit.dim(dimension*unit.dimension).exp(scale+unit.scale)
        }
        case ComplexUnitOfValue(expression,dimension,scale) if dimension>1 => Prod((for (a <- 1 to dimension.toInt) yield expression):_*)
        case e => e
    }
    
    private def cleanNumbersFx:(Expression)=>Expression = {
        case n:Number => ONE
        case e => e
    }
    
    /** Unit expression simplification rules */
    private def simplifyFx:(Expression)=>Expression = {
        
        // rules for translation
        
        case p:Prod => p.eval() match { case p2:Prod => p2.toProd2; case x => x }
        
        // rules for values simplification
        
        case Quot(n1:Value,n2:Value) => Quot(n1, n2).eval()    // evaluation
        case Prod2(n1:Value,n2:Value) => Prod2(n1, n2).eval()  // evaluation
        case Prod2(a,n1:Value) => Prod2(n1:Value,a)     // reordering
        case Prod2(ONE,a) => a                          //  1 * a = a
        //case Prod2(a,ONE) => a                        //  a * 1 = a
        case Prod2(ZERO,a) => ZERO                      //  0 * a = 0
        //case Prod2(a,ZERO) => ZERO                    //  a * 0 = 0
        case Quot(ZERO,a) => ZERO                       //  0/a = 0
        case Quot(a,ONE)  => a                          //  a/1 = a
        case Prod2(a,Quot(ONE,b)) => Quot(a,b)          //  a * 1/b = a/b
        //case Prod2(Quot(ONE,a),b) => Quot(b,a)          //  1/a * b = b/a
        
        //case Prod2(Quot(n1:Value,a),n2:Value) => Quot((n1*n2).eval,a)  //   n1/a * n2 = [n1*n2]/a
        //case Prod2(Quot(a,n1:Value),n2:Value) => Prod2(a,(n2/n1).eval) //   a/n1 * n2 = a*[n2/n1]
        case Prod2(n1:Value,Quot(n2:Value,a)) => Quot((n1 * n2).eval(),a)  //   n1 * n2/a = [n1*n2]/a
        case Prod2(n1:Value,Quot(a,n2:Value)) => Prod2(a,(n1 / n2).eval()) //   n1 * a/n2 = a*[n1/n2]
        
        //case Prod2(Prod2(n1:Value,a),n2:Value) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        //case Prod2(Prod2(a,n1:Value),n2:Value) => Prod2((n1*n2).eval,a)//   n1*a * n2 = [n1*n2]*a
        case Prod2(n1:Value,Prod2(n2:Value,a)) => Prod2((n1 * n2).eval(),a)//   n1*a * n2 = [n1*n2]*a
        case Prod2(n1:Value,Prod2(a,n2:Value)) => Prod2((n1 * n2).eval(),a)//   n1*a * n2 = [n1*n2]*a
        
        //case Quot(Quot(n1:Value,a),n2:Value) => Quot((n1/n2).eval,a) //   n1/a / n2 = [n1/n2]/a
        //case Quot(Quot(a,n1:Value),n2:Value) => Quot(a,(n1*n2).eval) //   a/n1 / n2 = a/[n1*n2]
        //case Quot(n1:Value,Quot(a,n2:Value)) => Quot((n1*n2).eval,a) //   n1 / a/n2 = [n1*n2]/a
        //case Quot(n1:Value,Quot(n2:Value,a)) => Prod2(a,(n1/n2).eval)//   n1 / n2/a = [n1/n2]*a
        
        //case Prod2(Quot(n1:Value,a),Quot(n2:Value,b)) => Quot((n1*n2).eval,Prod2(a,b)) //   n1/a * n2/b = [n1*n2]/[a*b]
        //case Prod2(Quot(n1:Value,a),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(b,a)) //   n1/a * b/n2 = [n1/n2]*[b/a]
        //case Prod2(Quot(a,n1:Value),Quot(n2:Value,b)) => Prod2((n2/n1).eval,Quot(a,b)) //   a/n1 * n2/b = [n2/n1]*[a/b]
        //case Prod2(Quot(a,n1:Value),Quot(b,n2:Value)) => Quot(Prod2(a,b),(n1/n2).eval) //   a/n1 * b/n2 = [a*b]/[n1*n2]
        
        case Quot(Prod2(n1:Value,a),Prod2(n2:Value,b)) => Prod2((n1 / n2).eval(),Quot(a,b)) //   n1*a / n2*b = [n1/n2]*[a/b]
        case Quot(Prod2(n1:Value,a),Prod2(b,n2:Value)) => Prod2((n1 / n2).eval(),Quot(a,b)) //   n1*a / b*n2 = [n1/n2]*[a/b]
        case Quot(Prod2(a,n1:Value),Prod2(n2:Value,b)) => Prod2((n1 / n2).eval(),Quot(a,b)) //   a*n1 / n2*b = [n1/n2]*[a/b]
        case Quot(Prod2(a,n1:Value),Prod2(b,n2:Value)) => Prod2((n1 / n2).eval(),Quot(a,b)) //   a*n1 / b*n2 = [n1/n2]*[a/b]
        
        case Prod2(Prod2(n1:Value,a),Prod2(n2:Value,b)) => Prod2((n1 * n2).eval(),Prod2(a,b)) // a*n1 * b*n2 = [n1*n2]*(a*b)
        
        //case Quot(Prod2(n1:Value,a),Quot(n2:Value,b)) => Prod2((n1/n2).eval,Prod2(a,b)) //   n1*a / n2/b = n1*a * b/n2 = [n1/n2]*[a*b]
        //case Quot(Prod2(n1:Value,a),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(a,b)) //   n1*a / b/n2 = n1*a * n2/b = [n1*n2]*[a/b]
        //case Quot(Prod2(a,n1:Value),Quot(n2:Value,b)) => Prod2((n1/n2).eval,Prod2(a,b)) //   a*n1 / n2/b = n1*a * b/n2 = [n1/n2]*[a*b]
        //case Quot(Prod2(a,n1:Value),Quot(b,n2:Value)) => Prod2((n1*n2).eval,Quot(a,b)) //   a*n1 / b/n2 = n1*a * n2/b = [n1*n2]*[a/b]
        
        //case Quot(Quot(n1:Value,a),Prod2(n2:Value,b)) => Quot((n1/n2).eval,Prod2(a,b)) //   n1/a / n2*b = n1 / n2*a*b = [n1/n2]/[a*b]
        //case Quot(Quot(n1:Value,a),Prod2(b,n2:Value)) => Quot((n1/n2).eval,Prod2(a,b)) //   n1/a / b*n2 = n1 / n2*a*b = [n1/n2]/[a*b]
        //case Quot(Quot(a,n1:Value),Prod2(n2:Value,b)) => Quot(Quot(a,b),(n1*n2).eval) //   a/n1 / n2*b = a / n2*n1*b = [a/b]/[n1*n2]
        //case Quot(Quot(a,n1:Value),Prod2(b,n2:Value)) => Quot(Quot(a,b),(n1*n2).eval) //   a/n1 / b*n2 = a / n2*n1*b = [a/b]/[n1*n2]
        
        // rules for units simplification

        case u if u eq EmptyUnitOfValue => ONE
        case ComplexUnitOfValue(expression,1,0) => expression
        case ComplexUnitOfValue(unit:UnitOfValue,dimension,scale) => dimension match {
            case 0 => EmptyUnitOfValue
            case 1 => unit
            case _ => unit.dim(dimension*unit.dimension).exp(scale+unit.scale)
        }
        case ComplexUnitOfValue(expression,dimension,scale) if dimension>1 => Prod((for (a <- 1 to dimension.toInt) yield expression):_*)
        case su:SimpleUnitOfValue if su.baseUnitName eq EmptyUnitOfValueName => su.multiplier
        
        case Power(u1:UnitOfValue,Number(Real(d),u2)) => u1.dim(u1.dimension*d)
        case root(u1:UnitOfValue,Number(Real(d),u2)) => u1.dim(u1.dimension/d)
        
        case Quot(u1:UnitOfValue,u2:UnitOfValue) if u1.isSameExpandedUnit(u2) => u1.expandedUnitMultiplier/u2.expandedUnitMultiplier
        case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyQuot(u1,u2)
        case Prod2(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) if u1.isSameBase(u2) => simplifyProd2(u1,u2)
        
        case Quot(Prod2(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2)) // (a*u1)/u2 = a*[u1/u2]
        case Quot(Prod2(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2)) // (u1*a)/u2 = a*[u1/u2]
        case Quot(u1:SimpleUnitOfValue,Prod2(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)  // u1/(a*u2) = [u1/u2]/a
        case Quot(u1:SimpleUnitOfValue,Prod2(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)  // u1/(u2*a) = [u1/u2]/a
        
        case Prod2(Prod2(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a)    //   (u1*a) * u2 = [n1*n2]*a
        case Prod2(Prod2(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a)    //   (a*u1) * u2 = [n1*n2]*a
        case Prod2(u1:SimpleUnitOfValue,Prod2(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a)    //   u1 * (a*u2) = [n1*n2]*a
        case Prod2(u1:SimpleUnitOfValue,Prod2(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(simplifyProd2(u1,u2),a)    //   u1 * (u2*a) = [n1*n2]*a
        
        case Prod2(Prod2(a,u1:SimpleUnitOfValue),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Prod2(a,b),simplifyProd2(u1,u2)) // a*n1 * b*n2 = [n1*n2]*(a*b)
        case Prod2(Prod2(u1:SimpleUnitOfValue,a),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Prod2(a,b),simplifyProd2(u1,u2)) // a*n1 * b*n2 = [n1*n2]*(a*b)
        case Prod2(Prod2(a,u1:SimpleUnitOfValue),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Prod2(a,b),simplifyProd2(u1,u2)) // a*n1 * b*n2 = [n1*n2]*(a*b)
        case Prod2(Prod2(u1:SimpleUnitOfValue,a),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Prod2(a,b),simplifyProd2(u1,u2)) // a*n1 * b*n2 = [n1*n2]*(a*b)
        
        case Prod2(Quot(u1:SimpleUnitOfValue,a),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyQuot(u1,u2))  // (u1/a) * (b/u2) = (b/a)*[u1/u2]
        case Prod2(Quot(a,u1:SimpleUnitOfValue),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))  // (a/u1) * (u2/b) = (a/b)*[u2/u1]
        case Prod2(Quot(u1:SimpleUnitOfValue,a),Quot(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),Prod2(a,b)) // (u1/a) * (u2/b) = [u1*u2]/(a*b)
        case Prod2(Quot(a,u1:SimpleUnitOfValue),Quot(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(Prod2(a,b),simplifyProd2(u1,u2)) // (a/u1) * (b/u2) = (a*b)/[u1*u2]
        
        case Quot(Prod2(a,u1:SimpleUnitOfValue),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2)) // (a*u1)/(b*u2) = (a/b)*[u1/u2]
        case Quot(Prod2(u1:SimpleUnitOfValue,a),Prod2(b,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2)) // (u1*a)/(b*u2) = (a/b)*[u1/u2]
        case Quot(Prod2(u1:SimpleUnitOfValue,a),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2)) // (a*u1)/(u2*b) = (a/b)*[u1/u2]
        case Quot(Prod2(a,u1:SimpleUnitOfValue),Prod2(u2:SimpleUnitOfValue,b)) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u1,u2)) // (u1*a)/(u2*b) = (a/b)*[u1/u2]
        
        case Prod2(Quot(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a) // (u1/a)*u2 = [u1*u2]/a
        case Prod2(u1:SimpleUnitOfValue,Quot(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a) // u1*(u2/a) = [u1*u2]/a
        case Prod2(Quot(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u2,u1)) // (a/u1)*u2 = a*[u2/u1]
        case Prod2(u1:SimpleUnitOfValue,Quot(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Prod2(a,simplifyQuot(u1,u2)) // u1*(a/u2) = a*[u1/u2]
        
        case Quot(Quot(a,u1:SimpleUnitOfValue),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(a,simplifyProd2(u1,u2)) // (a/u1)/u2 = a/[u1*u2]
        case Quot(Quot(u1:SimpleUnitOfValue,a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Quot(simplifyQuot(u1,u2),a)  // (u1/a)/u2 = [u1/u2]/a 
        case Quot(u1:SimpleUnitOfValue,Quot(a,u2:SimpleUnitOfValue)) if u1.isSameBase(u2) => Quot(simplifyProd2(u1,u2),a) // u1/(a/u2) = [u1*u2]/a
        case Quot(u1:SimpleUnitOfValue,Quot(u2:SimpleUnitOfValue,a)) if u1.isSameBase(u2) => Prod2(simplifyQuot(u1,u2),a) // u1/(u2/a) = [u1/u2]*a 
        
        case Prod2(Quot(a,Prod2(b,u1:SimpleUnitOfValue)),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))  // (a/(b*u1))*u2 = (a/b)*[u2/u1]
        case Prod2(Quot(a,Prod2(u1:SimpleUnitOfValue,b)),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))  // (a/(u1*b))*u2 = (a/b)*[u2/u1]
        case Prod2(Quot(Prod2(b,u1:SimpleUnitOfValue),a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyProd2(u2,u1)) // ((b*u1)/a)*u2 = (b/a)*[u1*u2]
        case Prod2(Quot(Prod2(u1:SimpleUnitOfValue,b),a),u2:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyProd2(u2,u1)) // ((u1*b)/a)*u2 = (b/a)*[u1*u2]
        
        case Prod2(u2:SimpleUnitOfValue,Quot(a,Prod2(b,u1:SimpleUnitOfValue))) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))  // u2*(a/(b*u1)) = (a/b)*[u2/u1]
        case Prod2(u2:SimpleUnitOfValue,Quot(a,Prod2(u1:SimpleUnitOfValue,b))) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyQuot(u2,u1))  // u2*(a/(u1*b)) = (a/b)*[u2/u1]
        case Prod2(u2:SimpleUnitOfValue,Quot(Prod2(b,u1:SimpleUnitOfValue),a)) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyProd2(u2,u1)) // u2*((b*u1)/a) = (b/a)*[u1*u2]
        case Prod2(u2:SimpleUnitOfValue,Quot(Prod2(u1:SimpleUnitOfValue,b),a)) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyProd2(u2,u1)) // u2*((u1*b)/a) = (b/a)*[u1*u2]
        
        case Prod2(u1:SimpleUnitOfValue,Prod2(Quot(u2:SimpleUnitOfValue,a),b)) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyProd2(u1,u2)) // u1*((u2/a)*b) = (b/a)*[u1*u2]
        case Prod2(u1:SimpleUnitOfValue,Prod2(a,Quot(u2:SimpleUnitOfValue,b))) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u1,u2)) // u1*(a*(u2/b)) = (a/b)*[u1*u2]
        case Prod2(Prod2(Quot(u2:SimpleUnitOfValue,a),b),u1:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(b,a),simplifyQuot(u2,u1))  // ((u2/a)*b)*u1 = (b/a)*[u2/u1]
        case Prod2(Prod2(a,Quot(u2:SimpleUnitOfValue,b)),u1:SimpleUnitOfValue) if u1.isSameBase(u2) => Prod2(Quot(a,b),simplifyProd2(u1,u2)) // (a*(u2/b))*u1 = (a/b)*[u2*u1]
        
        case Quot(u1:SimpleUnitOfValue,Prod2(a,Quot(b,u2:SimpleUnitOfValue))) if u1.isSameBase(u2) => Quot(Prod2(a,b),simplifyProd2(u1,u2)) // u1/(a*(b/u2)) = (a*b)/[u1*u2]
        
        case q:Quot if q.l.isInstanceOf[Prod2] && q.r.isInstanceOf[Prod2] => simplifyQuotOfProd(q)  // quot of prod simplification
        
        // rules for operation simplification
        
        case Quot(a,b) if a==b => ONE                                       // a/a = 1
        case Prod2(a,Prod2(b,Prod2(c,d))) => Prod2(Prod2(a,b),Prod2(c,d))   // (a*(b*(c*d))) = (a*b)*(c*d)
        case Quot(Quot(a,b),Quot(c,d)) => Prod2(Quot(a,b),Quot(d,c))        // (a/b) / (c/d) = (a/b)*(d/c)
        case Prod2(Quot(a,b),Quot(c,d)) => Quot(Prod2(a,c),Prod2(b,d))      // (a/b) * (c/d) = (a*c)/(b*d)
        case Quot(Quot(Prod2(a,b),c),d) => Quot(Prod2(a,b),Prod2(c,d))      // ((a*b)/c)/d) = (a*b)/(c*d)
        case Prod2(Prod2(a,b),Quot(c,d)) => Quot(Prod2(Prod2(a,b),c),d)     // (a*b)*(c/d) = ((a*b)*c)/d
        case Prod2(Quot(c,d),Prod2(a,b)) => Quot(Prod2(Prod2(a,b),c),d)     // (c/d)*(a*b) = ((a*b)*c)/d
        case Prod2(a,Quot(b,Prod2(c,d))) => Quot(Prod2(a,b),Prod2(c,d))     // a*(b/(c*d)) = (a*b)/(c*d)
        case Quot(Quot(a,b),c) => Quot(a,Prod2(b,c))                        // (a/b)/c = a/(b*c)
        case Quot(a,Quot(b,c)) => Quot(Prod2(a,c),b)                        // a/(b/c) = (a*c)/b
        case Prod2(Quot(a,b),c) => Quot(Prod2(a,c),b)                       // (a/b)*c = (a*c)/b
        case Prod2(c,Quot(a,b)) => Quot(Prod2(a,c),b)                       // c*(a/b) = (a*c)/b
        
        //case Prod2(p:Prod2,a) => Prod2(a,p)                               // reordering
        
        case e => e
    }
    
    private def simplifyQuot(u1:UnitOfValue,u2:UnitOfValue):Expression = {
        if(u1.isSameScale(u2)) {
            if(u1.dimension>=u2.dimension) u1.dim(u1.dimension-u2.dimension)
            else Quot(ONE,u1.dim(u2.dimension-u1.dimension))
        } 
        else {
            u1.multiplier/u2.multiplier match {
                case 1 if u1.dimension>=u2.dimension => u1.dim(u1.dimension-u2.dimension)
                case 1 if u1.dimension<u2.dimension => Quot(ONE,u1.dim(u2.dimension-u1.dimension))
                case m if m>1 && u1.dimension>=u2.dimension => Prod2(Number(Real(m)),u1.baseUnit.dim(u1.dimension-u2.dimension))
                case m if m<1 && u1.dimension>=u2.dimension => Quot(u1.baseUnit.dim(u1.dimension-u2.dimension),Number(Real(m).inverse))
                case m if m>1 && u1.dimension<u2.dimension => Quot(Number(Real(m)),u1.baseUnit.dim(u2.dimension-u1.dimension))
                case m if m<1 && u1.dimension<u2.dimension => Quot(ONE,Prod2(Number(Real(m).inverse),u1.baseUnit.dim(u2.dimension-u1.dimension)))
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
    
    private def simplifyQuotOfProd(q:Quot):Expression = {
        val l = q.l.asInstanceOf[Prod2].toProd.args.toSeq
        val r = q.r.asInstanceOf[Prod2].toProd.args.toSeq
        var seq2 = r
        val seq1 = l.collect {
            case u1:UnitOfValue => {
                val e = seq2.find({case u2:UnitOfValue => u2.isSameBase(u1); case _ => false})
                if(e.isDefined) {
                    seq2 = seq2.diff(Seq(e.get))
                    simplifyQuot(u1,e.get.asInstanceOf[UnitOfValue]) 
                } else u1
            }
            case e => e
        }.filter{case u if u eq EmptyUnitOfValue => false; case _ => true}
        seq2 = seq2.filter{case u if u eq EmptyUnitOfValue => false; case _ => true}
        if(l==seq1 && r==seq2) q
        else{
            if(seq2.isEmpty) {if(seq1.tail.isEmpty) seq1.head else Prod(seq1:_*)}
            else Quot(if(seq1.tail.isEmpty) seq1.head else Prod(seq1:_*),if(seq2.tail.isEmpty) seq2.head else Prod(seq2:_*))
        }
    }
}

case class NoUnit(e: Expression) extends Expression with Transparent {

    override val children: Seq[Expression] = Seq(e)

    final override def eval(): Expression = {
        val ev = e eval()
        ev match {
            case value: Value => value.setUnit(EmptyUnitOfValue)
            case _ if ev ne e => copy(ev)
            case _  => this
        }
    }

    final override def map(f: Transformation): Expression = {
        val ev = e.map(f)
        f(ev match {
            case value: Value => value.setUnit(EmptyUnitOfValue)
            case _ if ev ne e => copy(ev)
            case _  => this
        })
    }

}

case class SetUnit(e: Expression, override val unit:UnitOfValue) extends Expression with Transparent {

    override val children: Seq[Expression] = Seq(e)

    final override def eval(): Expression = {
        val ev = e eval()
        ev match {
            case value: Value => value.setUnit(unit)
            case _ if ev ne e => copy(ev)
            case _ if ev == e => this
        }
    }

    final override def map(f: Transformation): Expression = {
        val ev = e.map(f)
        f(ev match {
            case value: Value => value.setUnit(unit)
            case _ if ev ne e => copy(ev)
            case _ => this
        })
    }

}



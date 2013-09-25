package org.encalmo.expression


object Number {
    
    val typeId = 'Number

    //Numbers calculator registration
    NumberValueCalculator.doRegister()

}

/**
 * Real number value expression
 * @author artur.opala
 */
case class Number(

		private[encalmo] val r:Real,
		override val unit:UnitOfValue = EmptyUnitOfValue
		
	) extends Value {
    
    private[encalmo] var isRounded:Boolean = false
    private[encalmo] var original:Option[Real] = None

    val typeId = Number.typeId
	
    override def equals(a:Any):Boolean = a match {
        case n:Number if this.eq(n) => true
        case Number(real,u) => if((u eq unit) || u == unit || (u eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r==real else {
            if (u.isSameBaseAndDimension(this.unit)) real==this.r.convert1(this.unit,u)
            else if (u.isSameExpandedUnit(this.unit))real==this.r.convert2(this.unit,u)
            else false
        }
        case _ => false
    }
    
    override def convertTo(newunit:UnitOfValue):Number = newunit match {
        case u:UnitOfValue if this.unit eq u => this
        case u:UnitOfValue if newunit eq EmptyUnitOfValue => this
        case u:UnitOfValue if this.unit eq EmptyUnitOfValue => Number(r,newunit)
        case u:UnitOfValue if this.unit.isSameBaseAndDimension(u) => Number(r.convert1(unit,newunit),newunit)
        case u:UnitOfValue if this.unit.isSameExpandedUnit(u) => Number(r.convert2(unit,newunit),newunit)
        case _ => throw new IllegalValueConversionException(s"Could not convert value ${r.d} from ${this.unit.face} to ${newunit.face}")
    }
    
    override def convertTo(newunit:UnitOfValue,accuracy:Option[Double]):Number = newunit match {
        case u:UnitOfValue if this.unit eq u => this.convertTo(accuracy)
        case u:UnitOfValue if newunit eq EmptyUnitOfValue => this.convertTo(accuracy)
        case u:UnitOfValue if this.unit eq EmptyUnitOfValue => Number(r,newunit).convertTo(accuracy)
        case u:UnitOfValue if this.unit.isSameBaseAndDimension(u) => Number(r.convert1(unit,newunit),newunit).convertTo(accuracy)
        case u:UnitOfValue if this.unit.isSameExpandedUnit(u) => Number(r.convert2(unit,newunit),newunit).convertTo(accuracy)
        case _ => throw new IllegalValueConversionException(s"Could not convert value ${r.d} from ${this.unit.face} to ${newunit.face}")
    }
    
    private def roundedCopy(newr:Real):Number = {
        val n = Number(newr,unit)
        n.isRounded = true
        n.original = Some(r)
        n
    }

    override def convertTo(accuracy:Option[Double]):Number = accuracy match {
        case Some(a) => {
            val r2 = r.adjustValue(a)
            if(r2 ne r) roundedCopy(r2)  else this
        }
        case None => this
    }
    
    def adjustTo(accuracy:Double):Number = if(accuracy<=0) this else {
        val r2 = r.adjustValue(accuracy)
        if(r2 ne r) roundedCopy(r2) else this
    }
    
    override def convertToBaseUnit:Number = if(unit.isBaseUnit) this else Number(r.convert(unit,unit.baseUnit),unit.baseUnit)

    override def setUnit(newunit:UnitOfValue) = newunit match {
        case u if u eq unit => this
        case _ => copy(unit=newunit)
    }

	override def ^ (e:Expression):Expression = e match {
		case Number(r2,u2) if r2==Real.zero => ONE
		case Number(r2,u2) if r2==Real.one => this
		case _ => super.^(e)
	}

	override def unary_-():Expression = Number(-r,unit)

	override def +- (e:Expression):Expression = e match {
		case Number(r2,u2) if r2==Real.zero => this
		case _ => super.+-(e)
	}

	override def *- (e:Expression):Expression = e match {
		case Number(r2,u2) if r2==Real.zero => ZERO
		case Number(r2,u2) if r2==Real.one => Number(-r,unit)
		case _ => super.*-(e)
	}

	override def /- (e:Expression):Expression = e match {
		case Number(r2,u2) if r2==Real.one => Number(-r,unit)
		case _ => super./-(e)
	}

	override def ^- (e:Expression):Expression = e match {
		case Number(r2,u2) if r2==Real.zero => ONE
		case Number(r2,u2) if r2==Real.one => this
		case _ => super.^-(e)
	}

	override def < (e:Expression): Expression = e match {
	    case Number(r1,u1) =>  if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r<r1
    	    else {
    	        if(u1.isSameBaseAndDimension(unit)) r<r1.convert1(u1,unit)
    	        else if(u1.isSameExpandedUnit(this.unit)) r<r1.convert2(u1,unit) 
    	        else r<r1
    	    }
	    case _ => super.<(e)
    }
	override def > (e:Expression): Expression = e match {
	    case Number(r1,u1) => if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r>r1
            else {
                if(u1.isSameBaseAndDimension(unit)) r>r1.convert1(u1,unit) 
                else if(u1.isSameExpandedUnit(this.unit)) r>r1.convert2(u1,unit) 
                else r>r1
            }
	    case _ => super.>(e)
    }
	override def >= (e:Expression): Expression = e match {
	    case Number(r1,u1) => if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r>=r1
            else {
                if(u1.isSameBaseAndDimension(unit)) r>=r1.convert1(u1,unit) 
                else if(u1.isSameExpandedUnit(this.unit)) r>=r1.convert2(u1,unit) 
                else r>=r1
            }
	    case _ => super.>=(e)
    }
	override def <= (e:Expression): Expression = e match {
	    case Number(r1,u1) => if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r<=r1
            else {
                if(u1.isSameBaseAndDimension(unit)) r<=r1.convert1(u1,unit) 
                else if(u1.isSameExpandedUnit(this.unit)) r<=r1.convert2(u1,unit) 
                else r<=r1
            }
	    case _ => super.<=(e)
    }
	override def <> (e:Expression): Expression = e match {
	    case Number(r1,u1) => if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r<>r1
            else {
                if(u1.isSameBaseAndDimension(unit)) r<>r1.convert1(u1,unit) 
                else if(u1.isSameExpandedUnit(this.unit)) r<>r1.convert2(u1,unit) 
                else r<>r1
            }
	    case _ => super.<>(e)
    }
    override def === (e:Expression): Expression = e match {
        case Number(r1,u1) => if(u1==unit || (u1 eq EmptyUnitOfValue) || (this.unit eq EmptyUnitOfValue)) this.r == r1
        else {
            if(u1.isSameBaseAndDimension(unit)) r == r1.convert1(u1,unit)
            else if(u1.isSameExpandedUnit(this.unit)) r == r1.convert2(u1,unit)
            else r == r1
        }
        case _ => super.===(e)
    }

	def isInt = r.isInt
	def toInt:Int = r.toInt
	def toLong:Long = r.toLong
	def toDouble:Double = r.d
	
	def format:String = Real.format(r)
	def format(format:java.text.NumberFormat):String = Real.format(r, format)
	def format(locale:java.util.Locale):String = Real.format(r,locale)
	def format(pattern:String,locale:java.util.Locale):String = Real.format(r,pattern,locale)
	def asString:String = r.toString()
	
	/** Sets unit */
    override def unit(name:String):Number = copy(unit = SI(name).getOrElse(SimpleUnitOfValue(UnitOfValueName(name),0,1,SI)))
    override def unit(unit:UnitOfValue):Number = copy(unit = unit)
    override def set(unit: UnitOfValue): Number = copy(unit = unit)
    override def set(unit: String): Number = copy(unit = SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))

    def mapIfNotZero(doIfTrue: =>Expression):Expression = {
	    if(r.isZero) Void else doIfTrue
	}

    lazy val formattedToPrint: NumberFormatted = NumberFormatted.formatForPrint(this)
    def face = formattedToPrint.face + unit.face
	
}

/**
 * Object representation of the number Zero
 * @author artur.opala
 *
 */
object ZERO extends Number(Real.zero,EmptyUnitOfValue){

	override def + (e:Expression):Expression = e
	override def - (e:Expression):Expression = -e
	override def ^ (e:Expression):Expression = e match {
		case ZERO => ONE
		case _ => ZERO
	}

	override def unary_-():Expression = ZERO
	override def isInt = false

}

/**
 * Object representation of the number One
 * @author artur.opala
 *
 */
object ONE extends Number(Real.one,EmptyUnitOfValue){

	override def * (e:Expression):Expression = e
	override def % (e:Expression):Expression = e
	override def ^ (e:Expression):Expression = ONE
	override def isInt = true

}

object NumberValueCalculator extends ValueCalculator {
    
    import scala.Symbol
    
    def doRegister():Unit = {
        Value.register((Number.typeId,Number.typeId),this)
    }
    
    /** 
     * Calculates result of the operation with two arguments. 
     * Supports arguments of the Number type.
     */
    override def calculate(operator:Symbol, v1:Value, v2:Value):Option[Value] = {
        try {
        (v1,v2) match {
            case (Number(r1,u1),Number(r2,u2)) => {
                Option(operator match {
                    case '+ => {
                        if(u1==u2) Number(r1+r2,u1)
                        else if(u1.isSameBaseAndDimension(u2)){
                             if(u1.isLargerThan(u2)) Number(r1+ r2.convert1(u2, u1),u1) else Number(r1.convert1(u1,u2)+r2,u2)
                        }
                        else if (u1.isSameExpandedUnit(u2)){
                            if(u1.isLargerThan(u2)) Number(r1+ r2.convert2(u2, u1),u1) else Number(r1.convert2(u1,u2)+r2,u2)
                        }
                        else{
                            Number(r1+r2,u1+u2)
                        }
                    }
                    case '- => {
                        if(u1==u2) Number(r1-r2,u1)
                        else if(u1.isSameBaseAndDimension(u2)){ 
                             if(u1.isLargerThan(u2)) Number(r1- r2.convert1(u2, u1),u1) else Number(r1.convert1(u1,u2)-r2,u2)
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                             if(u1.isLargerThan(u2)) Number(r1- r2.convert2(u2, u1),u1) else Number(r1.convert2(u1,u2)-r2,u2)
                        }
                        else{
                            Number(r1-r2,u1-u2)
                        }
                    }
                    case '* => {
                        if(u1.isSameBase(u2)){
                             if(u1.isSameDimension(u2)){
                                 if(u1.isLargerScaleThan(u2)) 
                                     Number(r1* r2.convert1(u2, u1),u1.dim(u1.dimension*2))
                                     else Number(r1.convert1(u1,u2)*r2,u2.dim(u1.dimension*2))
                             }else if(u1.isSameScale(u2)){
                                 Number(r1*r2,u1.dim(u1.dimension+u2.dimension))
                             }else{
                                 if(u1.isLargerScaleThan(u2)) 
                                     Number(r1* r2.adjustScale(u2, u1),u1.dim(u1.dimension+u2.dimension))
                                     else Number(r1.adjustScale(u1, u2) * r2,u2.dim(u1.dimension+u2.dimension))
                             }
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                            if(u1.isLargerThan(u2)) {
                                val su = (u1*u1).simplifiedUnit
                                Number(r1* r2.convert2(u2, u1) *su._2,su._1)
                            } else {
                                val su = (u2*u2).simplifiedUnit
                                Number(r1.convert2(u1,u2)*r2*su._2,su._1)
                            }
                        }
                        else{
                            val su = (u1*u2).simplifiedUnit
                            Number(r1*r2*su._2,su._1)
                        }
                    }
                    case '/ => {
                       if(u1.isSameBase(u2)){
                             if(u1.isSameDimension(u2)){
                                 Number(r1/ r2.convert1(u2, u1),EmptyUnitOfValue)
                             }else if(u1.isSameScale(u2)){
                                 if(u1.dimension>u2.dimension) 
                                     Number(r1/r2,u1.dim(u1.dimension-u2.dimension)) 
                                     else Number(r1/r2,ComplexUnitOfValue(Quot(ONE,u1.dim(u2.dimension-u1.dimension))))
                             }else{
                                 if(u1.dimension>u2.dimension) {
                                     if(u1.isLargerScaleThan(u2)) 
                                         Number(r1/ r2.adjustScale(u2, u1),u1.dim(u1.dimension-u2.dimension))
                                         else Number(r1.adjustScale(u1, u2) / r2,u2.dim(u1.dimension-u2.dimension))
                                 }else{
                                     if(u1.isLargerScaleThan(u2)) 
                                         Number(r1/ r2.adjustScale(u2, u1),ComplexUnitOfValue(Quot(ONE,u1.dim(u2.dimension-u1.dimension))))
                                         else Number(r1.adjustScale(u1, u2) / r2,ComplexUnitOfValue(Quot(ONE,u2.dim(u2.dimension-u1.dimension))))
                                 }
                             }
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                            Number(r1/ r2.convert2(u2, u1),EmptyUnitOfValue)
                        }
                        else{
                            val su = (u1/u2).simplifiedUnit
                            Number(r1/r2*su._2,su._1)
                        }
                    }
                    case '% => {
                        if(u1==u2) Number(r1%r2,u1) 
                        else if(u1.isSameBaseAndDimension(u2)){
                             if(u1.isLargerThan(u2)) Number(r1.convert1(u1,u2)%r2,u2) else Number(r1% r2.convert1(u2, u1),u1)
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                            if(u1.isLargerThan(u2)) Number(r1.convert2(u1,u2)%r2,u2) else Number(r1% r2.convert2(u2, u1),u1)
                        }
                        else{
                            Number(r1%r2,u1%u2)
                        }
                    }
                    case '^ => {
                        if(r2>=0){
                            Number(r1^r2,u1.multiplyDimension(r2.d))
                        }else{
                            Number(r1^r2,EmptyUnitOfValue/ u1.multiplyDimension(-r2.d))
                        }
                    }
                    case 'root => {
                        Number(r1.root(r2),u1.divideDimension(r2.d))
                    }
                    case 'min => {
                        if(u1.isSameBaseAndDimension(u2)){
                             if(u1==u2) if (r1 <= r2) v1 else v2
                             else if(r1<=r2.convert1(u2,u1)) v1 else v2
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                             if(u1==u2) if (r1 <= r2) v1 else v2
                             else if(r1<=r2.convert2(u2,u1)) v1 else v2
                        }
                        else{
                             if(r1<=r2) v1 else v2
                        }
                    }
                    case 'max => {
                        if(u1.isSameBaseAndDimension(u2)){
                             if(u1==u2) if (r1 <= r2) v2 else v1
                             else if(r1<=r2.convert1(u2,u1)) v2 else v1
                        }
                        else if(u1.isSameExpandedUnit(u2)){ 
                            if(u1==u2) if (r1 <= r2) v2 else v1
                            else if(r1<=r2.convert2(u2,u1)) v2 else v1
                        }
                        else{
                             if(r1<=r2) v2 else v1
                        }
                    }
                    case 'hypot => Number(Real(java.lang.Math.hypot(r1.d,r2.convert(u2,u1).d)),u1)
                    case _ => null
                })
            }
            case _ => None
        }
        }
        catch {
            case exc:Exception => {
                Console.println(s"Cannot calculate number value: $operator($v1,$v2).\r\nCause: "+exc.getMessage)
                throw exc
            }
        }
    }
    
    /** 
     * Calculates result of the operation with two arguments. 
     * Supports arguments of the Number type. 
     */
    override def calculate(operator:scala.Symbol, v:Value):Option[Value] = {
        v match {
            case Number(r,u) => {
                Option(operator match {
                    case '- => Number(-r,u)
                    case 'sqrt => Number(r.sqrt,u.divideDimension(2))
                    case 'cbrt => Number(r.cbrt,u.divideDimension(3))
                    case 'exp => Number(r.exp)
                    case 'ln => Number(r.ln)
                    case 'log => Number(r.log)
                    case 'abs => Number(r.abs,u)
                    case 'sin => if(u==SI.rad) Number(r.sin) else r.d match {case 0 => 0; case 90 => 1; case 180 => 0; case 270 => -1; case _ => Number(r.rad.sin)}
                    case 'cos => if(u==SI.rad) Number(r.cos) else r.d match {case 0 => 1; case 90 => 0; case 180 => -1; case 270 => 0; case _ => Number(r.rad.cos)}
                    case 'tan => if(u==SI.rad) Number(r.tan) else Number(r.rad.tan)
                    case 'cot => if(u==SI.rad) Number(r.cot) else Number(r.rad.cot)
                    case 'arcsin => Number(r.arcsin.deg,SI.deg)
                    case 'arccos => Number(r.arccos.deg,SI.deg)
                    case 'arctan => Number(r.arctan.deg,SI.deg)
                    case 'arccot => Number(r.arccot.deg,SI.deg)
                    case 'sinh => if(u==SI.rad) Number(r.sinh) else Number(r.rad.sinh)
                    case 'cosh => if(u==SI.rad) Number(r.cosh) else Number(r.rad.cosh)
                    case 'tanh => if(u==SI.rad) Number(r.tanh) else Number(r.rad.tanh)
                    case 'coth => if(u==SI.rad) Number(r.coth) else Number(r.rad.coth)
                    case _ => null
                })
            }
            case _ => None
        }
    }
    
}
package org.encalmo.expression

import java.io.PrintWriter

/**
 * Real number value expression
 * @author artur.opala
 */
case class Number(

		private val r:Real,
		override val unit:UnitOfValue = EmptyUnitOfValue
		
	) extends Value {
	
	/** 
	 * Calculates result of the operation with two arguments. 
	 * Supports arguments of the Number type.
	 */
	override def calculate(operator:String, v1:Value, v2:Value):Option[Value] = {
		val r = (v1,v2) match {
			case (Number(r1,u1),Number(r2,u2)) => {
				Option(operator match {
					case "+" => {
					    if(u1.isSame(u2)){
					         if(u1==u2) Number(r1+r2,u1) else
					             if(u1.isLargerThan(u2)) Number(r1+(r2.convert(u2,u1)),u1) else Number(r1.convert(u1,u2)+r2,u2)
					    }else{
					        Number(r1+r2,u1+u2)
					    }
					}
					case "-" => {
					    if(u1.isSame(u2)){
                             if(u1==u2) Number(r1-r2,u1) else
                                 if(u1.isLargerThan(u2)) Number(r1-(r2.convert(u2,u1)),u1) else Number(r1.convert(u1,u2)-r2,u2)
                        }else{
                            Number(r1-r2,u1-u2)
                        }
					}
					case "*" => {
					    if(u1.isSame(u2)){
                             if(u1==u2) Number(r1*r2,u1.dim(u1.dimension+u2.dimension)) else
                                 if(u1.isLargerThan(u2)) Number(r1*(r2.convert(u2,u1)),u1.dim(u1.dimension+u2.dimension)) else Number(r1.convert(u1,u2)*r2,u2.dim(u1.dimension+u2.dimension))
                        }else{
                            Number(r1*r2,u1*u2)
                        }
					}
					case "/" => {
					    if(u1.isSame(u2)){
                             if(u1==u2) Number(r1/r2,u1.dim(u1.dimension-u2.dimension)) else
                                 if(u1.isLargerThan(u2)) Number(r1/(r2.convert(u2,u1)),u1.dim(u1.dimension-u2.dimension)) else Number(r1.convert(u1,u2)/r2,u2.dim(u1.dimension-u2.dimension))
                        }else{
                            Number(r1/r2,u1/u2)
                        }
					}
					case "%" => {
                        if(u1.isSame(u2)){
                             if(u1==u2) Number(r1%r2,u1) else
                                 if(u1.isLargerThan(u2)) Number(r1.convert(u1,u2)%r2,u2) else Number(r1%(r2.convert(u2,u1)),u1)
                        }else{
                            Number(r1%r2,u1%u2)
                        }
                    }
					case "^" => {
					    if(r2>=0){
					        Number(r1^r2,u1.dim(u1.dimension*r2.d))
					    }else{
					        Number(r1^r2,EmptyUnitOfValue/(u1.dim(u1.dimension*(-r2.d))))
					    }
					}
					case "root" => {
					    Number(r1.root(r2),u1.dim(u1.dimension/r2))
					}
					case "min" => {
					    if(u1.isSame(u2)){
                             if(u1==u2) ( if(r1<=r2) v1 else v2 ) 
                             else if(r1<=r2.convert(u2,u1)) v1 else v2
                        }else{
                             if(r1<=r2) v1 else v2
                        }
					}
					case "max" => {
                        if(u1.isSame(u2)){
                             if(u1==u2) ( if(r1<=r2) v2 else v1 ) 
                             else if(r1<=r2.convert(u2,u1)) v2 else v1
                        }else{
                             if(r1<=r2) v2 else v1
                        }
                    }
					case "hypot" => Number(Real(java.lang.Math.hypot(r1.d,r2.convert(u2,u1).d)),u1)
					case _ => null
				})
			}
			case _ => None
		}
		r
	}
	
	/** 
	 * Calculates result of the operation with two arguments. 
	 * Supports arguments of the Number type. 
	 */
	override def calculate(operator:String, v:Value):Option[Value] = {
		v match {
			case Number(r,u) => {
				Option(operator match {
					case "-" => Number(-r,u)
					case "sqrt" => Number(r.sqrt,u.dim(u.dimension/2))
					case "cbrt" => Number(r.cbrt,u.dim(u.dimension/3))
					case "exp" => Number(r.exp)
					case "ln" => Number(r.ln)
					case "log" => Number(r.log)
					case "abs" => Number(r.abs,u)
					case "sin" => if(u==SI.rad) Number(r.sin) else Number(r.rad.sin)
					case "cos" => if(u==SI.rad) Number(r.cos) else Number(r.rad.cos)
					case "tan" => if(u==SI.rad) Number(r.tan) else Number(r.rad.tan)
					case "cot" => if(u==SI.rad) Number(r.cot) else Number(r.rad.cot)
					case "arcsin" => Number(r.arcsin.deg,SI.deg)
					case "arccos" => Number(r.arccos.deg,SI.deg)
					case "arctan" => Number(r.arctan.deg,SI.deg)
					case "arccot" => Number(r.arccot.deg,SI.deg)
					case "sinh" => if(u==SI.rad) Number(r.sinh) else Number(r.rad.sinh)
					case "cosh" => if(u==SI.rad) Number(r.cosh) else Number(r.rad.cosh)
					case "tanh" => if(u==SI.rad) Number(r.tanh) else Number(r.rad.tanh)
					case "coth" => if(u==SI.rad) Number(r.coth) else Number(r.rad.coth)
					case _ => null
				})
			}
			case _ => None
		}
	}
	
    override def equals(a:Any):Boolean = a match {  
        case n:Number if this.eq(n) => true
        case Number(r,u) => if(u==unit || u == EmptyUnitOfValue || this.unit == EmptyUnitOfValue) this.r==r else {
            if (u.isSameBase(this.unit)) r==this.r.convert(this.unit,u)
            else false
        }
        case _ => false
    }
    
    override def convertTo(newunit:UnitOfValue):Number = newunit match {
        case u:UnitOfValue if this.unit==u => this
        case u:UnitOfValue if this.unit.isSameBase(u) => Number(r.convert(unit,newunit),newunit)
        case u:UnitOfValue if this.unit==EmptyUnitOfValue => Number(r,newunit)
        case _ => this
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

	override def < (e:Expression):Boolean = e match {case Number(r1,u1) => r<r1.convert(u1,unit); case _ => false}
	override def > (e:Expression):Boolean = e match {case Number(r1,u1) => r>r1.convert(u1,unit); case _ => false}
	override def >= (e:Expression):Boolean = e match {case Number(r1,u1) => r>=r1.convert(u1,unit); case _ => false}
	override def <= (e:Expression):Boolean = e match {case Number(r1,u1) => r<=r1.convert(u1,unit); case _ => false}
	override def <> (e:Expression):Boolean = e match {case Number(r1,u1) => r<>r1.convert(u1,unit); case _ => false}
	
	def isInt = r.isInt
	
	def format:String = Real.format(r)

	def format(format:java.text.NumberFormat):String = Real.format(r, format);

	def format(locale:java.util.Locale):String = Real.format(r,locale)

	def format(pattern:String,locale:java.util.Locale):String = Real.format(r,pattern,locale)
	
	/** Sets unit */
    def unit(name:String):Number = copy(unit = SI(name).getOrElse(SimpleUnitOfValue(UnitOfValueName(name),0,1,SI)))
    def unit(unit:UnitOfValue):Number = copy(unit = unit)
	
	private final def analyze(d:Double):(Long,Double) = {
		val ad = Math.abs(d)
		val fd = Math.floor(ad).toLong
		val fr = ad-fd
		if(fr+0.0001>=1) {
		    (fd+1,0)
		} else {
		    (fd,ad-fd)
		}
	}
		
	final def getScale(d:Double):Int = java.lang.Math.log10(d).toInt
	
	final lazy val formatForPrint:NumberFormatted = {
		val rif = analyze(r.d)
		val si:Int = if(rif._1==0) 0 else getScale(rif._1)
		val sf:Int = if(rif._2==0) 0 else Math.abs(getScale(rif._2))
		if(si>4){
			val nsi = (si%3)
			val nrif = analyze(rif._1/Math.pow(10,si-nsi))
			NumberFormatted(
				r.isNegative,
				true,
				nrif._1,
				nrif._2,
				si-nsi,
				2,
				EmptyUnitOfValue
			)
		}else{
			if(si==0){
				if(rif._1>0){
					NumberFormatted(
						r.isNegative,
						false,
						rif._1,
						rif._2,
						0,
						3,
						EmptyUnitOfValue
					)
				}else{
					if(sf>1){
						val nsf = (sf/3)*3+3
						val nrif = analyze(rif._2*Math.pow(10,nsf))
						if(nrif._2==0 && sf%3d==0){
							val n2rif = analyze(rif._2*Math.pow(10,nsf-3))
							NumberFormatted(
								r.isNegative,
								true,
								n2rif._1,
								n2rif._2,
								-(nsf-3),
								3,
								EmptyUnitOfValue
							)
						}else{
							NumberFormatted(
								r.isNegative,
								true,
								nrif._1,
								nrif._2,
								-nsf,
								3,
								EmptyUnitOfValue
							)
						}
					}else{
						NumberFormatted(
							r.isNegative,
							false,
							0,
							rif._2,
							0,
							3,
							EmptyUnitOfValue
						)
					}
				}
			}else{
				if(si+sf>6){
					NumberFormatted(
						r.isNegative,
						false,
						rif._1,
						0,
						0,
						0,
						EmptyUnitOfValue
					)
				}else{
					NumberFormatted(
						r.isNegative,
						false,
						rif._1,
						rif._2,
						0,
						2,
						EmptyUnitOfValue
					)
				}
			}
		}
	}
}

/**
 * Object representation of the number Zero
 * @author artur.opala
 *
 */
object ZERO extends Number(Real.zero){

	override def + (e:Expression):Expression = e
	override def - (e:Expression):Expression = -e
	override def ^ (e:Expression):Expression = e match {
		case _ if ZERO.eq(e) => ONE; 
		case _ => ZERO
	}

	override def unary_-():Expression = this
	override def isInt = false

}

/**
 * Object representation of the number One
 * @author artur.opala
 *
 */
object ONE extends Number(Real.one){

	override def * (e:Expression):Expression = e
	override def / (e:Expression):Expression = Quot(ONE,e)
	override def % (e:Expression):Expression = e
	override def ^ (e:Expression):Expression = ONE
	override def isInt = true

}
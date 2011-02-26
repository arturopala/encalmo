package org.encalmo.expression

import java.io.PrintWriter

/**
 * Real number expression
 * @author artur.opala
 */
case class Number(

		private val r:Real
		
	) extends Value {
	
	/** 
	 * Calculates result of the operation with two arguments. 
	 * Supports arguments of the Number type. 
	 */
	override def calculate(operator:String, v1:Value, v2:Value):Option[Value] = {
		val r = (v1,v2) match {
			case (Number(r1),Number(r2)) => {
				Option(operator match {
					case "+" => Number(r1+r2)
					case "-" => Number(r1-r2)
					case "*" => Number(r1*r2)
					case "/" => Number(r1/r2)
					case "%" => Number(r1%r2)
					case "^" => Number(r1^r2)
					case "root" => Number(r1.root(r2))
					case "min" => Number(Real(Math.min(r1.d,r2.d)))
					case "max" => Number(Real(Math.max(r1.d,r2.d)))
					case "hypot" => Number(Real(java.lang.Math.hypot(r1.d,r2.d)))
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
			case Number(r) => {
				Option(operator match {
					case "-" => Number(-r)
					case "sqrt" => Number(r.sqrt)
					case "cbrt" => Number(r.cbrt)
					case "exp" => Number(r.exp)
					case "ln" => Number(r.ln)
					case "log" => Number(r.log)
					case "abs" => Number(r.abs)
					case "sin" => Number(r.rad.sin)
					case "cos" => Number(r.rad.cos)
					case "tan" => Number(r.rad.tan)
					case "cot" => Number(r.rad.cot)
					case "arcsin" => Number(r.arcsin.deg)
					case "arccos" => Number(r.arccos.deg)
					case "arctan" => Number(r.arctan.deg)
					case "arccot" => Number(r.arccot.deg)
					case "sinh" => Number(r.rad.sinh)
					case "cosh" => Number(r.rad.cosh)
					case "tanh" => Number(r.rad.tanh)
					case "coth" => Number(r.rad.coth)
					case _ => null
				})
			}
			case _ => None
		}
	}

	override def ^ (e:Expression):Expression = e match {
		case Number(r2) if r2==Real.zero => ONE
		case Number(r2) if r2==Real.one => this
		case _ => super.^(e)
	}

	override def unary_-():Expression = Number(-r)

	override def +- (e:Expression):Expression = e match {
		case Number(r2) if r2==Real.zero => this
		case _ => super.+-(e)
	}

	override def *- (e:Expression):Expression = e match {
		case Number(r2) if r2==Real.zero => ZERO
		case Number(r2) if r2==Real.one => Number(-r)
		case _ => super.*-(e)
	}

	override def /- (e:Expression):Expression = e match {
		case Number(r2) if r2==Real.one => Number(-r)
		case _ => super./-(e)
	}

	override def %- (e:Expression):Expression = e match {
		case Number(r2) => Number(r%(-r2))
		case _ => super.%-(e)
	}

	override def ^- (e:Expression):Expression = e match {
		case Number(r2) if r2==Real.zero => ONE
		case Number(r2) if r2==Real.one => this
		case _ => super.^-(e)
	}

	override def < (e:Expression):Boolean = e match {case Number(r1) => r<r1; case _ => false}
	override def > (e:Expression):Boolean = e match {case Number(r1) => r>r1; case _ => false}
	override def >= (e:Expression):Boolean = e match {case Number(r1) => r>=r1; case _ => false}
	override def <= (e:Expression):Boolean = e match {case Number(r1) => r<=r1; case _ => false}
	override def <> (e:Expression):Boolean = e match {case Number(r1) => r<>r1; case _ => false}
	
	def isInt = r.isInt

	override def equals(a:Any):Boolean = a match {	
		case Number(r) => this.r==r
		case _ => false
	}
	
	def format:String = Real.format(r)

	def format(format:java.text.NumberFormat):String = Real.format(r, format);

	def format(locale:java.util.Locale):String = Real.format(r,locale)

	def format(pattern:String,locale:java.util.Locale):String = Real.format(r,pattern,locale)
	
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
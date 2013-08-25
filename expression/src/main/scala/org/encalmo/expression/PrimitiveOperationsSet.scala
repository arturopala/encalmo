package org.encalmo.expression

/**
 * Sum operation
 */
class Sum(val args:Expression*) extends MultipleInfixOperation {
	
	override def toString = "Sum("+args.mkString(",")+")"
	
	override def copy(x:Expression*):Sum = Sum(x:_*)
	override val operator = "+"
	override val precedence = 1
		
	override def + (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ZERO eq e => this
        case Sum(seq@_*) => Sum(args ++ seq:_*)
	  	case _ => Sum(args.:+(e):_*)
	}

    override def equals(a:Any):Boolean = a match {
        case Sum(seq@_*) => seq==args
        case _ => false
    }
    
    def toSum2:Expression = args.reduceLeft[Expression]((a,b) => Sum2(a,b))
}

object Sum {
    
    def apply(args:Expression*):Sum = {
        val pargs = if (!args.exists(a => a.isInstanceOf[Sum] || a.isInstanceOf[Sum2])) args else args.flatMap(_ match {
            case Sum(seq@_*) => seq
            case s2:Sum2 => s2.children
            case e => Seq(e)
        })
        new Sum(pargs:_*)
    }
    
    def unapplySeq(s:Sum):Option[Seq[Expression]] = Some(s.args)
 
}

/**
 * Simple sum operation with 2 arguments
 */
case class Sum2(l: Expression, r: Expression) extends InfixOperation {

    override def copy(l: Expression, r: Expression) = Sum2(l, r)
    override val operator = "+"
    override val precedence = 2
}

/**
 * Multiplication operation
 * @author artur.opala
 */
class Prod(val args:Expression*) extends MultipleInfixOperation {

	override def toString = "Prod("+args.mkString(",")+")"
	
	override def copy(x:Expression*):Prod = Prod(x:_*)
	override val operator = "*"
	override val precedence = 11
		
	override def * (e:Expression):Expression = e match {
	  	case Void => this
	  	case Unknown => Unknown
	  	case _ if ONE eq e => this
        case Prod(seq@_*) => Prod(args ++ seq:_*)
	  	case _ => Prod(args :+ e:_*)
	}
	
	override def equals(a:Any):Boolean = a match {
	    case Prod(seq@_*) => seq==args
	    case _ => false
	}
	
	def toProd2:Expression = args.reduceLeft[Expression]((a,b) => Prod2(a,b))
}

object Prod {
    
    def apply(args:Expression*):Prod = {
        val pargs = if (!args.exists(a => a.isInstanceOf[Prod] || a.isInstanceOf[Prod2])) args else args.flatMap(_ match {
            case Prod(seq@_*) => seq
            case p2:Prod2 => p2.children
            case e => Seq(e)
        })
        new Prod(pargs:_*)
    }
    
    def unapplySeq(p:Prod):Option[Seq[Expression]] = Some(p.args)
 
}

/**
 * Simple multiplication operation with 2 arguments
 */
case class Prod2(l: Expression, r: Expression) extends InfixOperation {

    override def copy(l: Expression, r: Expression) = Prod2(l, r)
    override val operator = "*"
    override val precedence = 11
    
    def toProd:Prod = Prod(back(l),back(r))
    
    private def back:(Expression)=>Expression = {
        case p:Prod2 => p.toProd
        case e => e
    }
}

/**
 * Difference operation
 */
case class Diff(l: Expression, r: Expression) extends InfixOperation {

	override def copy(l: Expression, r: Expression) = Diff(l, r)
	override val operator = "-"
	override val precedence = 2
}

/**
 * Division operation
 */
case class Quot(l: Expression, r: Expression) extends InfixOperation {

	override def copy(l: Expression, r: Expression) = Quot(l, r)
	override val operator = "/"
	override val precedence = 11
}

/**
 * Modulo operation
 */
case class Modulo(l: Expression, r: Expression) extends InfixOperation {

	override def copy(l: Expression, r: Expression) = Modulo(l, r)
	override val operator = "%"
		override val precedence = 12
}

/**
 * Negation operation
 */
case class Neg(e: Expression) extends PrefixOperation {

	override def copy(e: Expression) = Neg(e)
	override val operator = "-"
	override val precedence = 50
}

/**
 * Power operation
 */
case class Power(l: Expression, r: Expression) extends InfixOperation {

	override def copy(l: Expression, r: Expression) = Power(l, r)
	override def operator = "^"
	override val precedence = 50
}
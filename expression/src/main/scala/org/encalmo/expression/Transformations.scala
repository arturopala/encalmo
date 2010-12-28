package org.encalmo.expression

object Transformations {
	
	def simplifyProd(e:Expression):Expression = e match {
	    case Prod(l,r) if (ZERO.eq(l) || ZERO.eq(r)) => ZERO
	    case Prod(l,r) if (ONE.eq(l)) => r
	    case Prod(l,r) if (ONE.eq(r)) => l
	    case _ => e
	}
	
	def orderProd(e:Expression):Expression = e match {
	    case Prod(Number(l),Number(r)) if r<0 && l<0 => Prod(Number(l.abs),Number(r.abs))
	    case Prod(Number(l),Number(r)) if r<0 && l>0 => Prod(Number(r),Number(l))
	    case Prod(l,Number(r)) if r<0 && !(l.isInstanceOf[Neg]) => Prod(Number(r),l)
	    case Prod(Neg(e),Number(r)) if r<0 => Prod(Number(r.abs),e)
	    case Prod(Number(r),Neg(e)) if r<0 => Prod(Number(r.abs),e)
	    case Prod(Neg(l),Neg(r)) => Prod(l,r)
	    case _ => e
	}
	
	def simplifyQuot(e:Expression):Expression = e match {
	    case Quot(l,r) if (ZERO.eq(l)) => ZERO
	    case Quot(l,r) if (ONE.eq(r)) => l
	    case _ => e
	}
	  
	def orderQuot(e:Expression):Expression = e match {
	    case Quot(Number(l),Number(r)) if r<0 && l<0 => Quot(Number(l.abs),Number(r.abs))
	    case Quot(Neg(e),Number(r)) if r<0 => Quot(e,Number(r.abs))
	    case Quot(Number(r),Neg(e)) if r<0 => Quot(Number(r.abs),e)
	    case Quot(Neg(l),Neg(r)) => Quot(l,r)
	    case Quot(l,n:Number) => Prod(Quot(ONE,n),l)
	    case _ => e
	}
	
	def simplifySum(e:Expression):Expression = e match {
	    case Sum(l,r) if (ZERO.eq(l)) => r
	    case Sum(l,r) if (ZERO.eq(r)) => l
	    case Sum(Number(r1),Sum(Number(r2),e)) => Sum(e,Number(r1+r2))
	    case Sum(Number(r1),Sum(e,Number(r2))) => Sum(e,Number(r1+r2))
	    case Sum(Sum(Number(r2),e),Number(r1)) => Sum(e,Number(r1+r2))
	    case Sum(Sum(e,Number(r2)),Number(r1)) => Sum(e,Number(r1+r2))
	    case Sum(Number(r1),Diff(Number(r2),e)) => Diff(Number(r1+r2),e)
	    case Sum(Number(r1),Diff(e,Number(r2))) => Sum(e,Number(r1-r2))
	    case Sum(Diff(Number(r1),e),Number(r2)) => Sum(e,Number(r1+r2))
	    case Sum(Diff(e,Number(r1)),Number(r2)) => Diff(e,Number(r1+r2))
	    case _ => e
	}
	  
	def orderSum(e:Expression):Expression = e match {
	    case Sum(l,Number(r)) if (r.isNegative) => Diff(l,Number(-r))
	    case Sum(l,Neg(r)) => Diff(l,r)
	    case Sum(Number(r),l) if !(l.isInstanceOf[Number]) => Sum(l,Number(r))
	    case _ => e
	}
	  
	def simplifyDiff(e:Expression):Expression = e match {
	    case Diff(l,r) if (ZERO.eq(r)) => l
	    case Diff(l,r) if (ZERO.eq(l)) => Neg(r)
	    case Diff(Number(r1),Diff(Number(r2),e)) => Sum(Number(r1-r2),e)
	    case Diff(Number(r1),Diff(e,Number(r2))) => Diff(Number(r1+r2),e)
	    case _ => e
	}
	  
	def orderDiff(e:Expression):Expression = e match {
	    case Diff(l,Number(r)) if (r.isNegative) => Sum(l,Number(-r))
	    case Diff(l,Neg(r)) => Sum(l,r)
	    case Diff(l,Diff(r1,r2)) => Sum(Diff(l,r1),r2)
	    case _ => e
	}
	  
	def simplifyModulo(e:Expression):Expression = e match {
	    case Modulo(l,r) if (ZERO.eq(l)) => ZERO
	    case Modulo(l,r) if (ONE.eq(r)) => l
	    case _ => e
	}
	  
	def simplifyPower(e:Expression):Expression = e match {
	    case Power(l,r) if (ZERO.eq(r)) => ONE
	    case Power(l,r) if (ZERO.eq(l)) => ZERO
	    case Power(l,r) if (ONE.eq(r)) => l
	    case _ => e
	}

}
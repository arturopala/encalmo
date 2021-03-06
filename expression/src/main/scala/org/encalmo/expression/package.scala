package org.encalmo

import scala.language.implicitConversions

package object expression {
	
	/** Alias for transforming (mapping) function */
	type Transformation = Expression=>Expression
	type tg = tan
	type ctg = cot
	type ctn = cot
	type arctg = arctan
	type arcctg = arccot
	type arcctn = arccot
	
	val One = ONE
	val Zero = ZERO
	
	implicit def real2Double(r:Real):Double = r.d
	implicit def real2Long(r:Real):Long = Math.round(r.d)
	implicit def real2Number(r:Real):Number = r match{case Real(d) if d==0 =>ZERO; case Real(d) if d==1 =>ONE; case _=>Number(r)}
	
	implicit def int2Real(i:Int):Real = new Real(i)
	implicit def long2Real(l:Long):Real = new Real(l)
	implicit def float2Real(f:Float):Real = new Real(f)
	implicit def double2Real(d:Double):Real = new Real(d)
	  
	implicit def int2Number(i:Int):Number = i match{case _ if i==0 =>ZERO; case _ if i==1 =>ONE; case _=> Number(new Real(i))}
	implicit def long2Number(l:Long):Number = l match{case _ if l==0 =>ZERO; case _ if l==1 =>ONE; case _=> Number(new Real(l))}
	implicit def float2Number(f:Float):Number = f match{case _ if f==0 =>ZERO; case _ if f==1 =>ONE; case _=> Number(new Real(f))}
	implicit def double2Number(d:Double):Number = d match{case _ if d==0 =>ZERO; case _ if d==1 =>ONE; case _=> Number(new Real(d))}
	  
	implicit def exprFx2Expression(m:()=>Expression):Expression = new ExprFx(m)
	implicit def realFx2Expression(m:()=>Real):Expression = new RealFx(m)
	
	implicit def string2Symbol(string:String):Symbol = Symbol(string)

    implicit def booleanValue2Boolean(b:BooleanValue):Boolean = b.boolean
    implicit def boolean2BooleanValue(b:Boolean):BooleanValue = BooleanValue(b)
    implicit def expression2BooleanOpt(e:Expression):Option[Boolean] = e match {
        case BooleanValue(b) => Some(b)
        case op:BooleanOperation => expression2BooleanOpt(op.eval())
        case _ => None}
    implicit def expression2Boolean(e:Expression):Boolean = e match {
        case BooleanValue(b) => b
        case op:BooleanOperation => expression2Boolean(op.eval())
        case _ => false}
	
	val console:java.io.PrintWriter = new java.io.PrintWriter(Console.out)
	
	def dumpRaw(e:Expression*) = e.foreach(e => {
			console.println(e)
			console.flush()
		}
	)
	
	//helper functions
	def frac(m:Int,d:Int) = Quot(m,d)
	//function definition
	def fx(expr:Expression, vars:Symbol*) = Function(expr,vars:_*)
	//Text value expression
	def text(s:String):TextValue = TextValue(s)
	// Power with exponent 2
	def square(e:Expression):Power = Power(e,Number(2))
    def sq(e:Expression):Power = Power(e,Number(2))
	// Power with exponent 3
	def cube(e:Expression):Power = Power(e,Number(3))
    def cb(e:Expression):Power = Power(e,Number(3))
	// Rounding down with accuracy
	def floor(e:Expression,accuracy:Double = 1) = round(e,RoundingMode.Step(false,accuracy))
	// Rounding up with accuracy
	def ceil(e:Expression) = round(e,RoundingMode.CEILING)
	def ceil(e:Expression,accuracy:Double = 1) = round(e,RoundingMode.Step(true,accuracy))
    // average
    def avg(e1:Expression,e2:Expression):Quot = Quot(Sum(e1,e2),Number(2))
	
	def rangeChoiceLE(e:Expression,first:Expression,bound:Expression,second:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLessThanOrEqualTo(e,bound)),Case(CaseExpression(second),IsGreaterThan(e,bound))))
	}
	
	def rangeChoiceGE(e:Expression,first:Expression,bound:Expression,second:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLowerThan(e,bound)),Case(CaseExpression(second),IsGreaterThanOrEqualTo(e,bound))))
	}
	
	def rangeChoiceLLE(e:Expression,first:Expression,lowerBound:Expression,second:Expression,upperBound:Expression,third:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLowerThan(e,lowerBound)),Case(CaseExpression(second),IsInRangeLessOrEqualAndLessThan(lowerBound,e,upperBound)),Case(CaseExpression(third),IsGreaterThanOrEqualTo(e,upperBound))))
	}
	
	def rangeChoiceLEL(e:Expression,first:Expression,lowerBound:Expression,second:Expression,upperBound:Expression,third:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLessThanOrEqualTo(e,lowerBound)),Case(CaseExpression(second),IsInRangeLessAndLessOrEqual(lowerBound,e,upperBound)),Case(CaseExpression(third),IsGreaterThan(e,upperBound))))
	}
	
	def rangeChoiceLL(e:Expression,first:Expression,lowerBound:Expression,second:Expression,upperBound:Expression,third:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLowerThan(e,lowerBound)),Case(CaseExpression(second),IsInRangeLessOrEqualAndLessOrEqual(lowerBound,e,upperBound)),Case(CaseExpression(third),IsGreaterThan(e,upperBound))))
	}
	
	def rangeChoiceLELE(e:Expression,first:Expression,lowerBound:Expression,second:Expression,upperBound:Expression,third:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLessThanOrEqualTo(e,lowerBound)),Case(CaseExpression(second),IsInRangeLessAndLess(lowerBound,e,upperBound)),Case(CaseExpression(third),IsGreaterThanOrEqualTo(e,upperBound))))
	}
	
	def rangeChoice4LE(e:Expression,first:Expression,bound1:Expression,second:Expression,bound2:Expression,third:Expression,bound3:Expression,fourth:Expression):Expression = {
		Selection(Seq(Case(CaseExpression(first),IsLessThanOrEqualTo(e,bound1)),Case(CaseExpression(second),IsInRangeLessAndLessOrEqual(bound1,e,bound2)),Case(CaseExpression(third),IsInRangeLessAndLessOrEqual(bound2,e,bound3)),Case(CaseExpression(fourth),IsGreaterThan(e,bound3))))
	}
	
	def mapChoice(e:Expression,mapppings: (Expression,Expression)*):Expression = {
		Selection(mapppings.map(x => Case(CaseExpression(x._2),IsEqualTo(e,x._1))))
	}

    def assertEqualTo(left:Expression,right:Expression): Assert = Assert(left,Relation.EQUAL,right)
    def assertLessThen(left:Expression,right:Expression): Assert = Assert(left,Relation.LESS,right)
    def assertLessThenOrEqualTo(left:Expression,right:Expression): Assert = Assert(left,Relation.LESS_OR_EQUAL,right)
    def assertGreaterThen(left:Expression,right:Expression): Assert = Assert(left,Relation.GREATER,right)
    def assertGreaterThenOrEqualTo(left:Expression,right:Expression): Assert = Assert(left,Relation.GREATER_OR_EQUAL,right)
	
}
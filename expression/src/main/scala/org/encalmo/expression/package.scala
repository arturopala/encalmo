package org.encalmo

package object expression {
	
	/** Alias for transforming (mapping) function */
	type Transformation = Expression=>Expression
	type tg = tan
	type ctg = cot
	type ctn = cot
	type arctg = arctan
	type arcctg = arccot
	type arcctn = arccot
	
	val One = one
	val Zero = zero
	
	implicit def real2Double(r:Real):Double = r.d
	implicit def real2Long(r:Real):Long = Math.round(r.d)
	implicit def real2Number(r:Real):Number = r match{case Real(d) if d==0 =>zero; case Real(d) if d==1 =>one; case _=>Number(r)}
	
	implicit def int2Real(i:Int):Real = new Real(i)
	implicit def long2Real(l:Long):Real = new Real(l)
	implicit def float2Real(f:Float):Real = new Real(f)
	implicit def double2Real(d:Double):Real = new Real(d)
	  
	implicit def int2Number(i:Int):Number = i match{case _ if i==0 =>zero; case _ if i==1 =>one; case _=>Number(new Real(i))}
	implicit def long2Number(l:Long):Number = l match{case _ if l==0 =>zero; case _ if l==1 =>one; case _=>Number(new Real(l))}
	implicit def float2Number(f:Float):Number = f match{case _ if f==0 =>zero; case _ if f==1 =>one; case _=>Number(new Real(f))}
	implicit def double2Number(d:Double):Number = d match{case _ if d==0 =>zero; case _ if d==1 =>one; case _=>Number(new Real(d))}
	  
	implicit def exprFx2Expression(m:()=>Expression):Expression = new ExprFx(m)
	implicit def realFx2Expression(m:()=>Real):Expression = new RealFx(m)
	
	implicit def string2Symbol(string:String):Symbol = Symbol(string)
	implicit def char2Symbol(char:Char):Symbol = Symbol(char)
	
	val console:java.io.PrintWriter = new java.io.PrintWriter(Console.out)
	
	def dumpRaw(e:Expression*) = e.foreach(e => {
			console.println(e)
			console.flush
		}
	)
	
}
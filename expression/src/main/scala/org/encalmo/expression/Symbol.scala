package org.encalmo.expression

/**
 * Symbol
 * @author artur.opala
 */
trait Symbol extends Expression {
	
	def name:String
	def subscript:Symbol = null
	def superscript:Symbol = null
	def underscript:Symbol = null
	def overscript:Symbol = null
	
	val face:String = name + forFace(subscript) + forFace(superscript) + forFace(underscript) + forFace(overscript)
  
	private def forFace(script:Symbol) = if(script!=null) "{"+script.face+"}" else ""
		
	def |(sub:Symbol):Symbol
	def |(sub:Symbol, sup:Symbol):Symbol
	def |(int:Int):Symbol = this | String.valueOf(int)
	def |(sub:Symbol,int2:Int):Symbol = this | (sub,String.valueOf(int2))
	def |(int1:Int,sup:Symbol):Symbol = this | (String.valueOf(int1),sup)
	def |(int1:Int,int2:Int):Symbol = this | (String.valueOf(int1),String.valueOf(int2))
	def !(sup:Symbol):Symbol
	def !(int:Int):Symbol = this ! String.valueOf(int)
	def +(n:String):Symbol
	
	def under(under:Symbol):Symbol
	def over(over:Symbol):Symbol
	
	def hasSubscript:Boolean = subscript!=null
	def hasSuperscript:Boolean = superscript!=null
	def hasSubOrSupscript:Boolean = hasSubscript || hasSuperscript
	def hasSubAndSupscript:Boolean = hasSubscript && hasSuperscript
	def hasUnderscript:Boolean = underscript!=null
	def hasOverscript:Boolean = overscript!=null
	def hasOverOrUnderscript:Boolean = hasUnderscript || hasOverscript
	def hasOverAndUnderscript:Boolean = hasUnderscript && hasOverscript
	
}

object Symbol {
	
	def apply(char:Char):Symbol = new Symbol1(String.valueOf(char))
	def apply(name:String):Symbol1 = new Symbol1(name)
  	def apply(name:String,subscript:Symbol):Symbol2 = new Symbol2(name,subscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol):Symbol3 = new Symbol3(name,subscript,superscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol):Symbol4 = new Symbol4(name,subscript,superscript,underscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol,overscript:Symbol):Symbol5 = new Symbol5(name,subscript,superscript,underscript,overscript)
  
}

case class Symbol1(name:String) extends Symbol {
	override def toString = "Symbol("+name+")"
	override def +(n:String):Symbol1 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol2 = Symbol2(name, s)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, null, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, null, null, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, null, null, null, over)
}

case class Symbol2(name:String, override val subscript:Symbol) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+")"
	override def +(n:String):Symbol2 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol = Symbol2(name, s)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, subscript, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, null, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, null, null, over)
}

case class Symbol3(name:String, override val subscript:Symbol, override val superscript:Symbol) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+")"
	override def +(n:String):Symbol3 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol3 = Symbol3(name, s, superscript)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, subscript, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, null, over)
}

case class Symbol4(name:String, override val subscript:Symbol, override val superscript:Symbol, override val underscript:Symbol) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+","+underscript+")"
	override def +(n:String):Symbol4 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol4 = Symbol4(name, s, superscript, underscript)
	override def |(sub:Symbol,sup:Symbol):Symbol4 = Symbol4(name, sub, sup, underscript)
	override def !(sup:Symbol):Symbol4 = Symbol4(name, subscript, sup, underscript)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, underscript, over)
} 

case class Symbol5(name:String, override val subscript:Symbol, override val superscript:Symbol, override val underscript:Symbol, override val overscript:Symbol) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+","+underscript+","+overscript+")"
	override def +(n:String):Symbol5 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol5 = Symbol5(name, s, superscript, underscript, overscript)
	override def |(sub:Symbol,sup:Symbol):Symbol5 = Symbol5(name, sub, sup, underscript, overscript)
	override def !(sup:Symbol):Symbol5 = Symbol5(name, subscript, sup, underscript, overscript)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, underscript, over)
} 
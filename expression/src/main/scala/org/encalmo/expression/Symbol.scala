package org.encalmo.expression

import scala.collection.immutable.StringOps

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
    
    def contextId:Option[Seq[String]] = None

    def |(sub:Symbol):Symbol
    def |(sub:Symbol, sup:Symbol):Symbol
    def !(sup:Symbol):Symbol
    def +(n:String):Symbol
    def under(under:Symbol):Symbol
    def over(over:Symbol):Symbol
    def at(id:String):Symbol

    def !(int:Int):Symbol = this ! String.valueOf(int)
    def |(int:Int):Symbol = this | String.valueOf(int)
    def |(sub:Symbol,int2:Int):Symbol = this | (sub,String.valueOf(int2))
    def |(int1:Int,sup:Symbol):Symbol = this | (String.valueOf(int1),sup)
    def |(int1:Int,int2:Int):Symbol = this | (String.valueOf(int1),String.valueOf(int2))
    
    def hasSubscript:Boolean = subscript!=null
    def hasSuperscript:Boolean = superscript!=null
    def hasSubOrSupscript:Boolean = hasSubscript || hasSuperscript
    def hasSubAndSupscript:Boolean = hasSubscript && hasSuperscript
    def hasUnderscript:Boolean = underscript!=null
    def hasOverscript:Boolean = overscript!=null
    def hasOverOrUnderscript:Boolean = hasUnderscript || hasOverscript
    def hasOverAndUnderscript:Boolean = hasUnderscript && hasOverscript

    lazy val face:String = name + forFace(subscript) + forFace(superscript) + forFace(underscript) + forFace(overscript)
    lazy val face2:String = new StringOps(name).filter(_ match {
            case ',' => false
            case '/' => false
            case '.' => false
            case _ => true
        }) + forFace2(subscript) + forFace2(superscript) + forFace2(underscript) + forFace2(overscript)
    
    private def forFace(script:Symbol) = if(script!=null) "{"+script.face+"}" else ""
    private def forFace2(script:Symbol) = if(script!=null) script.face2 else ""

    def is(description:String):SymbolWithDescription = SymbolWithDescription(this,description)

}

/**
 * Symbol proxy abstract class
 * @author artur.opala
 */
abstract class SymbolProxy(val symbol:Symbol) extends Symbol {
    
    override def name:String = symbol.name
    override def subscript:Symbol = symbol.subscript
    override def superscript:Symbol = symbol.superscript
    override def underscript:Symbol = symbol.underscript
    override def overscript:Symbol = symbol.overscript
    override def contextId:Option[Seq[String]] = symbol.contextId
}

/**
 * Symbol with description and unit
 * @author artur.opala
 */
case class SymbolWithDescription(override val symbol:Symbol, val description:String, val unit:String = "") extends SymbolProxy(symbol) {
    
    def unit(unit:String):SymbolWithDescription = SymbolWithDescription(symbol,description,unit)
   
    override def |(sub:Symbol):Symbol = copy(symbol = symbol | sub)
    override def |(sub:Symbol, sup:Symbol):Symbol = copy(symbol = symbol | (sub,sup))
    override def !(sup:Symbol):Symbol = copy(symbol = symbol | sup)
    override def +(n:String):Symbol = copy(symbol = symbol + n)
    override def under(under:Symbol):Symbol = copy(symbol = symbol.under(under))
    override def over(over:Symbol):Symbol = copy(symbol = symbol.over(over))
    override def at(id:String):Symbol = copy(symbol = symbol.at(id))
    
}

/**
 * Symbol companion object
 * @author artur.opala
 */
object Symbol {
	
	def apply(char:Char):Symbol = new Symbol1(String.valueOf(char))
	def apply(name:String):Symbol1 = new Symbol1(name)
  	def apply(name:String,subscript:Symbol):Symbol2 = new Symbol2(name,subscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol):Symbol3 = new Symbol3(name,subscript,superscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol):Symbol4 = new Symbol4(name,subscript,superscript,underscript)
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol,overscript:Symbol):Symbol5 = new Symbol5(name,subscript,superscript,underscript,overscript)
  
}

case class Symbol1(name:String, override val contextId:Option[Seq[String]] = None) extends Symbol {
	override def toString = "Symbol("+name+")"
	override def +(n:String):Symbol1 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol2 = Symbol2(name, s)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, null, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, null, null, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, null, null, null, over)
	override def at(id:String):Symbol1 = copy(contextId = (contextId match {case None => Some(Seq(id)); case Some(seq) => Some(seq :+ id)}))
}

case class Symbol2(name:String, override val subscript:Symbol, override val contextId:Option[Seq[String]] = None) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+")"
	override def +(n:String):Symbol2 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol = Symbol2(name, s)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, subscript, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, null, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, null, null, over)
	override def at(id:String):Symbol2 = copy(contextId = (contextId match {case None => Some(Seq(id)); case Some(seq) => Some(seq :+ id)}))
}

case class Symbol3(name:String, override val subscript:Symbol, override val superscript:Symbol, override val contextId:Option[Seq[String]] = None) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+")"
	override def +(n:String):Symbol3 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol3 = Symbol3(name, s, superscript)
	override def |(sub:Symbol,sup:Symbol):Symbol3 = Symbol3(name, sub, sup)
	override def !(sup:Symbol):Symbol3 = Symbol3(name, subscript, sup)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, null, over)
	override def at(id:String):Symbol3 = copy(contextId = (contextId match {case None => Some(Seq(id)); case Some(seq) => Some(seq :+ id)}))
}

case class Symbol4(name:String, override val subscript:Symbol, override val superscript:Symbol, override val underscript:Symbol, override val contextId:Option[Seq[String]] = None) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+","+underscript+")"
	override def +(n:String):Symbol4 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol4 = Symbol4(name, s, superscript, underscript)
	override def |(sub:Symbol,sup:Symbol):Symbol4 = Symbol4(name, sub, sup, underscript)
	override def !(sup:Symbol):Symbol4 = Symbol4(name, subscript, sup, underscript)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, underscript, over)
	override def at(id:String):Symbol4 = copy(contextId = (contextId match {case None => Some(Seq(id)); case Some(seq) => Some(seq :+ id)}))
} 

case class Symbol5(name:String, override val subscript:Symbol, override val superscript:Symbol, override val underscript:Symbol, override val overscript:Symbol, override val contextId:Option[Seq[String]] = None) extends Symbol {
	override def toString = "Symbol("+name+","+subscript+","+superscript+","+underscript+","+overscript+")"
	override def +(n:String):Symbol5 = this.copy(name = name+n)
	override def |(s:Symbol):Symbol5 = Symbol5(name, s, superscript, underscript, overscript)
	override def |(sub:Symbol,sup:Symbol):Symbol5 = Symbol5(name, sub, sup, underscript, overscript)
	override def !(sup:Symbol):Symbol5 = Symbol5(name, subscript, sup, underscript, overscript)
	override def under(under:Symbol):Symbol4 = Symbol4(name, subscript, superscript, under)
	override def over(over:Symbol):Symbol5 = Symbol5(name, subscript, superscript, underscript, over)
	override def at(id:String):Symbol5 = copy(contextId = (contextId match {case None => Some(Seq(id)); case Some(seq) => Some(seq :+ id)}))
} 
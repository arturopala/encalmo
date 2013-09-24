package org.encalmo.expression

import org.encalmo.common.Translator
import scala.collection.immutable.StringOps

/**
 * Symbol of expression
 * @author artur.opala
 */
class Symbol(
		
	val name:String,
    val subscript:Option[Symbol] = None,
    val superscript:Option[Symbol] = None,
    val underscript:Option[Symbol] = None,
    val overscript:Option[Symbol] = None,
    val description:Option[String] = None,
    override val unit:UnitOfValue = EmptyUnitOfValue,
    val dictionary:Option[String] = None,
    val contextId:Option[String] = None,
    override val printable:Boolean = true,
    val args:Option[Seq[Symbol]] = None,
    val indexes:Option[Seq[Int]] = None,
    val accuracy:Option[Double] = None

) extends Expression with SymbolLike {

	override def symbol:Symbol = this

	/** Sets subscript */
    def |(sub:Symbol):Symbol = copy(subscript = Option(sub))
    /** Sets subscript and superscript */
    def |(sub:Symbol, sup:Symbol):Symbol = copy(subscript = Option(sub),superscript = Option(sup))
    /** Sets superscript */
    def !(sup:Symbol):Symbol = copy(superscript = Option(sup))
    /** Append string to the name */
    def +(n:String):Symbol = copy(name = name+n)
    /** Prepend string to the name */
    def +:(n:String):Symbol = copy(name = n+name)
    /** Sets underscript */
    def under(under:Symbol):Symbol = copy(underscript = Option(under))
    /** Sets overscript */
    def over(over:Symbol):Symbol = copy(overscript = Option(over))
    /** Append string to the subscript */
    def |+(n:String):Symbol = copy(subscript = Option(subscript match { case Some(s) => s+(","+n); case None => n}))
     /** Append string to the superscript */
    def !+(n:String):Symbol = copy(superscript = Option(superscript match { case Some(s) => s+(","+n); case None => n}))
    /** Sets arguments */
    def args (symbols:Symbol*):Symbol = copy(args = Option(symbols))
    /** Sets indexes */
    def apply(indexes:Int*):Symbol = copy(indexes = Option(indexes))

    def !(int:Int):Symbol = this ! String.valueOf(int)
    def |(int:Int):Symbol = this | String.valueOf(int)
    def |(sub:Symbol,int2:Int):Symbol = this | (sub,String.valueOf(int2))
    def |(int1:Int,sup:Symbol):Symbol = this | (String.valueOf(int1),sup)
    def |(int1:Int,int2:Int):Symbol = this | (String.valueOf(int1),String.valueOf(int2))
    
    def hasSubscript:Boolean = subscript.isDefined
    def hasSuperscript:Boolean = superscript.isDefined
    def hasSubOrSupscript:Boolean = hasSubscript || hasSuperscript
    def hasSubAndSupscript:Boolean = hasSubscript && hasSuperscript
    def hasUnderscript:Boolean = underscript.isDefined
    def hasOverscript:Boolean = overscript.isDefined
    def hasOverOrUnderscript:Boolean = hasUnderscript || hasOverscript
    def hasOverAndUnderscript:Boolean = hasUnderscript && hasOverscript
    def hasArgs:Boolean = args.isDefined && !args.get.isEmpty
    def hasIndexes:Boolean = indexes.isDefined && !indexes.get.isEmpty
    
    def hasDescription:Boolean = description.isDefined
    def hasUnit:Boolean = unit.isDefined
    def hasDictionary:Boolean = dictionary.isDefined
    
    /** Returns translated description */
    def localizedDescription(locale:java.util.Locale):Option[String] = {
        concatenate(
            Translator.translate(face,locale,dictionary),
            Translator.translate(description,locale,dictionary).orElse(description)
        )
    }

    /** Returns true if exists translated description of this Symbol */
    def hasLocalizedDescription(locale:java.util.Locale):Boolean = {
        Translator.hasTranslation(face,locale,dictionary) || Translator.hasTranslation(description,locale,dictionary)
    }
    
    /** Unique symbol face */
    override lazy val face: String = name +
    	Seq(toFace(overscript),toFace(underscript),toFace(superscript),toFace(subscript))
    	.foldLeft[Option[String]](None)((l,r) => l match {
    		case None => r
    		case Some(sl) => r match {
    			case None => Some("{}"+sl)
    			case Some(sr) => Some(sr+sl)
    		}
    	}).getOrElse("") + toArgsFace(args)
    	
    /** Simplified symbol face */
    lazy val simpleFace: String = new StringOps(name).filter(_ match {
            case ',' => false
            case '/' => false
            case '.' => false
            case _ => true
        }) + toFace2(subscript) + toFace2(superscript) + toFace2(underscript) + toFace2(overscript) + toArgsFace2(args)  + toIndexesFace2(indexes)
        
    lazy val simpleFaceNoArgs: String = new StringOps(name).filter(_ match {
            case ',' => false
            case '/' => false
            case '.' => false
            case _ => true
        }) + toFace2(subscript) + toFace2(superscript) + toFace2(underscript) + toFace2(overscript) + toIndexesFace2(indexes)
        
    /** Unique symbol face */
    private def toFace(s:Option[Symbol]):Option[String] = if(s.isDefined) Some("{"+s.get.face+"}") else None
    /** Simplified symbol face */
    private def toFace2(s:Option[Symbol]) = if(s.isDefined) s.get.simpleFace else ""
    /** Unique args face */
    private def toArgsFace(s:Option[Seq[Symbol]]) =  if(s.isDefined && !s.get.isEmpty) {
        "(" + s.get.foldLeft[String]("")((l,r) => if(l.isEmpty) r.face else l+","+r.face) + ")"
    } else ""    
    /** Simplified args face */ 
    private def toArgsFace2(s:Option[Seq[Symbol]]) =  if(s.isDefined && !s.get.isEmpty) {
        "(" + s.get.foldLeft[String]("")((l,r) => if(l.isEmpty) r.simpleFace else l+","+r.simpleFace) + ")"
    } else ""
    /** Simplified indexes face */     
    private def toIndexesFace2(s:Option[Seq[Int]]) =  if(s.isDefined && !s.get.isEmpty) {
        "[" + s.get.foldLeft[String]("")((l,r) => if(l.isEmpty) r.toString else l+","+r) + "]"
    } else ""
    	
    /** Adds id to the contextId sequence */
    def id(id:String):Symbol = copy(contextId = Option(id))
    /** Sets description */
    def is(description:String):Symbol = copy(description = Option(description))
    /** Appends comment to the description */
    override def ##(comment:String):Symbol = copy(description = concatenate(description,Option(comment)))
    override def ##(comment:Option[String]):Symbol = copy(description = concatenate(description,comment))
    /** Sets unit */
    def unit(unit:String):Symbol = copy(unit = SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))
    def unit(unit:UnitOfValue):Symbol = copy(unit = unit)
    /** Set accuracy of evaluations */
    def accuracy(d:Double) = copy(accuracy = Option(d))
    def acc(d:Double) = copy(accuracy = Option(d)) // short name
    def accuracy(optd:Option[Double]) = copy(accuracy = optd)
    /** Sets dictionary */
    def dict(dict:String):Symbol = copy(dictionary = Option(dict))
    def dictionary(dictOpt:Option[String]):Symbol =  copy(dictionary = dictOpt)
    /** Copy entire symbol or selected attributes*/
    def copy(
		name:String = this.name,
	    subscript:Option[Symbol] = this.subscript,
	    superscript:Option[Symbol] = this.superscript,
	    underscript:Option[Symbol] = this.underscript,
	    overscript:Option[Symbol] = this.overscript,
	    description:Option[String] = this.description,
	    unit:UnitOfValue = this.unit,
	    dictionary:Option[String] = this.dictionary,
	    contextId:Option[String] = this.contextId,
	    printable:Boolean = this.printable,
	    args:Option[Seq[Symbol]] = this.args,
	    indexes:Option[Seq[Int]] = this.indexes,
	    accuracy:Option[Double] = this.accuracy
	) = {
		new Symbol(name,subscript,superscript,underscript,overscript,description,unit,dictionary,contextId,printable,args,indexes,accuracy)
    }
    /** Sets this symbol as non printable */
    def makeNonPrintable:Symbol = copy(printable = false)
    
    override def equals(a:Any):Boolean = {
    	a match {
    		case s:Symbol => {
                (s eq this) || (
    			s.name == name &&
		    	(s.contextId == contextId) &&
		    	(s.subscript == subscript) &&
		    	(s.superscript == superscript) &&
		    	(s.overscript == overscript) &&
		    	(s.underscript == underscript) &&
		    	(s.args == args) &&
		    	(s.indexes == indexes))
    		}
    		case _ => false
    	}
    }
    
    override lazy val hashCode:Int = {
		val prime = 31
        var result = 1
        result = prime * result + name.hashCode
		subscript.map(x => result = prime * result + x.hashCode)
		superscript.map(x => result = prime * result + x.hashCode)
		underscript.map(x => result = prime * result + x.hashCode)
		overscript.map(x => result = prime * result + x.hashCode)
		contextId.map(x => result = prime * result + x.hashCode)
		args.map(x => result = prime * result + x.hashCode)
		indexes.map(x => result = prime * result + x.hashCode)
		result
	}
    
    override def toString:String = string
    	
	private lazy val string:String = {
    	val sb = new StringBuilder
    	sb.append("Symbol(name=\"")
    	sb.append(name)
    	sb.append("\"")
    	subscript.map(x => {sb.append(",subscript=");sb.append(x.toString)})
    	superscript.map(x => {sb.append(",superscript=");sb.append(x.toString)})
    	underscript.map(x => {sb.append(",underscript=");sb.append(x.toString)})
    	overscript.map(x => {sb.append(",overscript=");sb.append(x.toString)})
    	description.map(x => {sb.append(",description=\"");sb.append(x.toString);sb.append("\"")})
    	if(unit.isDefined){sb.append(",unit=\"");sb.append(unit.toString);sb.append("\"")}
    	dictionary.map(x => {sb.append(",dictionary=\"");sb.append(x.toString);sb.append("\"")})
    	contextId.map(x => {sb.append(",contextId=");sb.append(x.toString())})
		sb.append(")")
		sb.toString()
    }

}

/**
 * Symbol companion object
 * @author artur.opala
 */
object Symbol {
	
	def apply(char:Char):Symbol = new Symbol(String.valueOf(char))
	def apply(i:Int):Symbol = new Symbol(String.valueOf(i))
	def apply(name:String):Symbol = new Symbol(name)
  	def apply(name:String,subscript:Symbol):Symbol = new Symbol(name,Option(subscript))
	def apply(name:String,subscript:Symbol,superscript:Symbol):Symbol = new Symbol(name,Option(subscript),Option(superscript))
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol):Symbol = new Symbol(name,Option(subscript),Option(superscript),Option(underscript))
	def apply(name:String,subscript:Symbol,superscript:Symbol,underscript:Symbol,overscript:Symbol):Symbol = new Symbol(name,Option(subscript),Option(superscript),Option(underscript),Option(overscript))
  
	def apply(
		name:String,
	    subscript:Option[Symbol] = None,
	    superscript:Option[Symbol] = None,
	    underscript:Option[Symbol] = None,
	    overscript:Option[Symbol] = None,
	    description:Option[String] = None,
	    unit:UnitOfValue = EmptyUnitOfValue,
	    dictionary:Option[String] = None,
	    contextId:Option[String] = None,
	    printable:Boolean = true,
	    args:Option[Seq[Symbol]] = None,
        indexes:Option[Seq[Int]] = None,
        accuracy:Option[Double] = None
	) = {
		new Symbol(name,subscript,superscript,underscript,overscript,description,unit,dictionary,contextId,printable,args,indexes,accuracy)
	}
	
	def unapply(s:Symbol) = Some(s.name,s.subscript,s.superscript)
	
}
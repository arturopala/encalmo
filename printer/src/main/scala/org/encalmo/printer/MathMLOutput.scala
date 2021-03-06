package org.encalmo.printer

import org.encalmo.expression._
import org.encalmo.style.Style
import org.encalmo.style.FontStyle
import org.encalmo.style.DefaultStyle
import org.encalmo.common.Translator
import MathMLTags._

/**
 * MathML text output
 * @author artur.opala
 */
class MathMLOutput(
	locale:java.util.Locale = java.util.Locale.getDefault, 
	namespace:String = "ml", 
	buffer:StringBuilder = new StringBuilder,
	indent:Indent = new Indent(2),
	declare:Boolean = true,
	displayType:Option[String] = None,
	printStyles:Boolean = true
) 
extends XmlTextOutput(locale, namespace, buffer, indent) {
	
	var mathStyle:Style = DefaultStyle
	var numberStyle:Style = null

	val integerFormat1:java.text.NumberFormat = new java.text.DecimalFormat("#",decimalFormatSymbols)
	val dimensionFormat:java.text.NumberFormat = new java.text.DecimalFormat("0.#",decimalFormatSymbols)
	val fractionFormat1:java.text.NumberFormat = new java.text.DecimalFormat(".#",decimalFormatSymbols)
	val fractionFormat2:java.text.NumberFormat = new java.text.DecimalFormat(".##",decimalFormatSymbols)
	val fractionFormat3:java.text.NumberFormat = new java.text.DecimalFormat(".###",decimalFormatSymbols)
	val fractionFormat4:java.text.NumberFormat = new java.text.DecimalFormat(".####",decimalFormatSymbols)
	
	override def open() = {
		startNoIndent(MATH)
		if(declare){
		    declareNamespace("http://www.w3.org/1998/Math/MathML")
		}
		if(displayType.isDefined){
		    attr("display",displayType.get)
		}
		attr("scriptsizemultiplier","0.95")
		attr("scriptminsize","6pt")
		body()
		if(printStyles && mathStyle!=null){
			start(MSTYLE)
			appendStyleAttributes(mathStyle)
			body()
		}
	}
	
	override def close() = {
		if(printStyles && mathStyle!=null){
			end(MSTYLE)
		}
		endNoIndent(MATH)
	}
	
	def thickspace() = {
		start(MSPACE)
		attr("width",THICKMATHSPACE)
		end()
	}

	def thinspace() = {
		start(MSPACE)
		attr("width",THINMATHSPACE)
        end()
	}

	def leftBracket() = {
		start(MFENCED)
		attr("open","(")
		attr("close",")")
		attr("separators",";")
		body()
		startb(MROW)
	}

	def rightBracket() = {
		end(MROW)
		end(MFENCED)
	}
	
	def separator() = {
		end(MROW)
		startb(MROW)
	}

	def mo(s:String): Unit = mo(s, null)

	def mo(s:String, form:String): Unit = mo(s, form, null, null)

	def mo(s:String, form:String, lspace:String, rspace:String): Unit = {
		start(MO)
		if (form != null) attr("form",form)
		if (lspace != null) attr("lspace",lspace)
		if (rspace != null) {
			attr("rspace",rspace)
		}
		body()
		buffer append convert(s)
		end(MO)
	}

	def mn(n: Number):Unit = {
		val nf:NumberFormatted = n.formattedToPrint
		if(nf.hasExponent || nf.isNegative) {
			start(MROW)
			if(numberStyle!=null){
				appendStyleAttributes(numberStyle)
			}
			body()
		}
		val omit = !nf.isNegative && nf.integer==1 && nf.fraction==0 && nf.exponent!=0
		if(!omit){
			if (nf.isNegative) {
				mo("-", "prefix")
			}
			start(MN)
			if(!nf.hasExponent && !nf.isNegative){
				if(numberStyle!=null){
					appendStyleAttributes(numberStyle)
				}
			}
			attr("class",NUM_CLASS_ID)
			body()
            append(nf.integer.toString)
			if(nf.fraction>0){
				nf.decimals match {
					case 1 => append(fractionFormat1.format(nf.fraction))
					case 2 => append(fractionFormat2.format(nf.fraction))
					case 3 => append(fractionFormat3.format(nf.fraction))
					case _ => append(fractionFormat4.format(nf.fraction))
				}
			}
			end(MN)
		}
		if(nf.hasExponent && nf.exponent!=0) {
			if(!omit){
				start(MO)
				attr("class","numexp")
				attr("mathsize",resolveStyle.font.size-2)
				body()
				append(ENTITY_CENTER_DOT)
				end(MO)
			}
			start(MSUP)
			attr("mathsize",resolveStyle.font.size-2)
			attr("class","numexp")
			body()
			startb(MN)
			append("10")
			end(MN)
			if(nf.exponent<0){
				startb(MROW)
				mo("-", "prefix")
			}
			startb(MN)
			append(Math.abs(nf.exponent).toString)
			end(MN)
			if(nf.exponent<0){
				end(MROW)
			}
			end(MSUP)
		}
		if(nf.hasExponent || nf.isNegative) {
			end(MROW)
		}
	}

	def mi(s:String):Unit = {
		startb(MI)
		append(s)
		end(MI)
	}

	def mi(s:String, size: Int = 0, variant:String=null):Unit = {
		start(MI)
		if(size>0){
		    attr("mathsize",size,"%")
		}
		if(variant!=null){
		    attr("mathvariant",variant)
		}
		body()
		append(s)
		end(MI)
	}
	
	def mtextClass(classId:String,s:String*):Unit = {
		start(MTEXT)
		attr("class",classId)
		body()
		s.foreach(append(_))
		end(MTEXT)
	}

	def mtext(s:String*):Unit = {
		startb(MTEXT)
		s.foreach(append(_))
		end(MTEXT)
	}

	def mtext(s:String, size: Int):Unit = {
		start(MTEXT)
		if(size>0) attr("mathsize",size,"%")
		body()
		append(s)
		end(MTEXT)
	}
	
	def mtext2(s:String, lspace:String=null,rspace:String=null, size: Int = 0):Unit = {
		start(MTEXT)
		if(size>0) attr("mathsize",size,"%")
		body()
		if (lspace != null) append(lspace)
		append(s)
		if (rspace != null) append(rspace)
		end(MTEXT)
	}
	
	def msup(s1:String,s2:String,lspace:String=null,rspace:String=null,size:Int = 0){
		start(MSUP)
		if(size>0) attr("mathsize",size,"%")
		body()
		if (lspace != null) append(lspace)
		mtext(s1)
		mtext(s2)
		if (rspace != null) append(rspace)
		end(MSUP)
	}
	
	def symbol(s:Symbol,script:Boolean=false,printArgs:Boolean=true,classId:String = SYMB_CLASS_ID):Unit = {
	    start(MROW)
	    if(printStyles && !script){
    	    attr("mathvariant","italic")
	    }
	    if(classId!=null){
	        attr("class",classId)
	    }
	    body()
		//under-over script start
		if (s.hasOverAndUnderscript) startb(MUNDEROVER)
		else if (s.hasOverscript) startb(MOVER)
		else if (s.hasUnderscript) startb(MUNDER)
		//sub-super script start
		if (s.hasSubAndSupscript || (s.hasSuperscript && s.hasIndexes)) startb(MSUBSUP)
		else if (s.hasSuperscript) startb(MSUP)
		else if (s.hasSubscript || s.hasIndexes) startb(MSUB)
		//core name
		if(!script){
		    mi(BasicSymbols.toMathML(s.name))
		}else{
		    mi(BasicSymbols.toMathML(s.name))
		}
		//scripts
		if(s.hasSubscript && s.hasIndexes) startb(MROW)
		if (s.hasSubscript) symbol(s.subscript.get,true,true,SYMB_SUB_CLASS_ID)
		if(s.hasIndexes) {
		    start(MTEXT)
		    attr("mathsize","90%")
		    //attr("mathvariant","italic")
		    body()
            append("[")
            var ic = 0
            s.indexes.get.foreach(i => {
                if(ic>0){
                    append(",")
                }
                 append(i.toString)
                ic = ic+1
            })
            append("]")
            end(MTEXT)
		}
		if(s.hasSubscript && s.hasIndexes) end(MROW)
		if (s.hasSuperscript) symbol(s.superscript.get,true,true,SYMB_SUB_CLASS_ID)
		if (s.hasSubAndSupscript || (s.hasSuperscript && s.hasIndexes)) end(MSUBSUP)
		else if (s.hasSuperscript) end(MSUP)
		else if (s.hasSubscript || s.hasIndexes) end(MSUB)
		//sub-super script end
		if (s.hasUnderscript) symbol(s.underscript.get,true,true,SYMB_UNDER_CLASS_ID)
		if (s.hasOverscript) symbol(s.overscript.get,true,true,SYMB_OVER_CLASS_ID)
		if (s.hasOverAndUnderscript) end(MUNDEROVER)
		else if (s.hasOverscript) end(MOVER)
		else if (s.hasUnderscript) end(MUNDER)
		if(printArgs && s.hasArgs){
		    start(MROW,SYMB_ARGS_CLASS_ID)
		    attr("mathsize","85%")
		    body()
            startb(MTEXT)
		    append("(")
		    end(MTEXT)
		    var ic = 0
		    s.args.get.foreach(s => {
		        if(ic>0){
		            startb(MTEXT)
		            append(",")
		            end(MTEXT)
		        }
		        symbol(s)
		        ic = ic+1
		    })
		    startb(MTEXT)
		    append(")")
		    end(MTEXT)
		    end(MROW)
		}
		//under-over script end
		end(MROW)
	}
	
	def unit(u:SimpleUnitOfValue):Unit = {
	    u.dimension match {
	        case 1 => mtext(u.name.toString)
	        case _ => {
	            startb(MSUP)
				mtext(u.name.toString)
				startb(MN,"pwri")
				append(dimensionFormat.format(u.dimension))
				end(MN)
				end(MSUP)
	        }
	    }
	}
	
	def appendStyleAttributes(style:Style) = {
	    if(printStyles){
			attr("mathcolor",style.hexColor)
			attr("mathbackground",style.hexBackground)
			attr("mathsize",style.font.size)
			attr("mathvariant",resolveFontVariant(style.font))
	    }
	}
	
	private def resolveStyle = if(numberStyle!=null) numberStyle else mathStyle
	
	private def resolveFontVariant(fs:FontStyle):String = {
		fs match {
			case `fs` if fs.italic && fs.bold => "bold-italic"
			case `fs` if fs.italic => "italic"
			case `fs` if fs.bold => "bold"
			case _ => "normal"
		}
	}

    def convert(s:String):String = s match {
        case "-" => "&minus;"
        case "+" => "+"
        case "*" => ENTITY_CENTER_DOT
        case ">" => "&gt;"
        case "<" => "&lt;"
        case "<=" => "&le;"
        case ">=" => "&geq;"
        case "!=" => "&ne;"
        case "!= 0" => "&ne; 0"
        case "= 0" => "&equiv; 0"
        case "=" => "="
        case "≈" => "&asymp;"
        case "≃" => "&sime;"
        case "~" => "&sim;"
        case "⇒" => "&rArr;"
        case _ => s
    }

    def translate(s:String): String = {
        Translator.translate(s,locale,Translator.defaultDictionary).getOrElse(s)
    }
	
}
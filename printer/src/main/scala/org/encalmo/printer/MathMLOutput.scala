package org.encalmo.printer
import java.io.Writer
import org.encalmo.expression._
import org.encalmo.document.Style
import org.encalmo.document.FontStyle
import org.encalmo.document.DefaultStyle
import MathMLTags._

/**
 * MathML text output
 * @author artur.opala
 */
class MathMLOutput(
	locale:java.util.Locale = java.util.Locale.getDefault, 
	namespace:String = "ml", 
	buffer:StringBuilder = new StringBuilder,
	indent:Indent = new Indent(2)
) 
extends XmlTextOutput(locale, namespace, buffer, indent) {
	
	var mathStyle:Style = DefaultStyle
	var numberStyle:Style = null
	var prefix:String = null
	var suffix:String = null
	
	lazy val integerFormat1:java.text.NumberFormat = new java.text.DecimalFormat("###,###,###,###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat1:java.text.NumberFormat = new java.text.DecimalFormat(".#",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat2:java.text.NumberFormat = new java.text.DecimalFormat(".##",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat3:java.text.NumberFormat = new java.text.DecimalFormat(".###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat4:java.text.NumberFormat = new java.text.DecimalFormat(".####",java.text.DecimalFormatSymbols.getInstance(locale))
	
	override def open = {
		start(MATH)
		declareNamespace("http://www.w3.org/1998/Math/MathML")
		//attr("displaystyle","true")
		//attr("scriptlevel","0")
		//attr("scriptsizemultiplier","0.5")
		//attr("scriptminsize","3pt")
		body
		//start(MROW)
		//body
		if(prefix!=null & prefix!=""){
			startb(MTEXT)
			append(ENTITY_THICK_SPACE)
			append(prefix)
			append(ENTITY_THICK_SPACE)
			end(MTEXT)
		}
		if(mathStyle!=null){
			start(MSTYLE)
			appendStyleAttributes(mathStyle)
			body
		}
	}
	
	override def close = {
		if(mathStyle!=null){
			end(MSTYLE)
		}
		if(suffix!=null & suffix!=""){
			start(MTEXT)
			attr("mathsize",resolveStyle.font.size-2)
			body
			append(ENTITY_THICK_SPACE)
			append(suffix)
			append(ENTITY_THICK_SPACE)
			end(MTEXT)
		}
		//end(MROW)
		end(MATH)
		prefix = null
		suffix = null
	}
	
	def thickspace = {
		start(MSPACE)
		attr("width","thickmathspace")
		body
	}

	def thinspace = {
		start(MSPACE)
		attr("width","thinmathspace")
		body
	}

	def leftBracket = {
		start(MFENCED)
		attr("open","(")
		attr("close",")")
		attr("separators",";")
		body
		startb(MROW)
	}

	def rightBracket = {
		end(MROW)
		end(MFENCED)
	}
	
	def separator = {
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
		body
		buffer append (s match {
		case "-" => "&minus;"
		case "+" => "+"
		case "*" => ENTITY_CENTER_DOT
		case _ => s
		})
		end(MO)
	}

	def mn(n: Number):Unit = {
		val nf:NumberFormatted = n.formatForPrint
		if(nf.hasExponent || nf.isNegative) {
			start(MROW)
			if(numberStyle!=null){
				appendStyleAttributes(numberStyle)
			}
			body
		}
		if (nf.isNegative) {
			mo("-", "prefix")
		}
		start(MN)
		if(!nf.hasExponent && !nf.isNegative){
			if(numberStyle!=null){
				appendStyleAttributes(numberStyle)
			}
		}
		body
		append(integerFormat1.format(nf.integer))
		if(nf.fraction>0){
			nf.decimals match {
				case 1 => append(fractionFormat1.format(nf.fraction))
				case 2 => append(fractionFormat2.format(nf.fraction))
				case 3 => append(fractionFormat3.format(nf.fraction))
				case _ => append(fractionFormat4.format(nf.fraction))
			}
		}
		end(MN)
		if(nf.hasExponent && nf.exponent!=0) {
			start(MO)
			attr("mathsize",resolveStyle.font.size-2)
			body
			append(ENTITY_CENTER_DOT)
			end(MO)
			start(MSUP)
			attr("mathsize",resolveStyle.font.size-2)
			body
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

	def mi(s:String, size: Int):Unit = {
		start(MI)
		attr("mathsize",size,"%")
		body
		append(s)
		end(MI)
	}

	def mtext(s:String*):Unit = {
		startb(MTEXT)
		s.foreach(append(_))
		end(MTEXT)
	}

	def mtext(s:String, size: Int):Unit = {
		start(MTEXT)
		attr("mathsize",size,"%")
		body
		append(s)
		end(MTEXT)
	}
	
	def symbol(s:Symbol):Unit = {
		//under-over script start
		if (s.hasOverAndUnderscript) startb(MUNDEROVER)
		else if (s.hasOverscript) startb(MOVER)
		else if (s.hasUnderscript) startb(MUNDER)
		//sub-super script start
		if (s.hasSubAndSupscript) startb(MSUBSUP)
		else if (s.hasSuperscript) startb(MSUP)
		else if (s.hasSubscript) startb(MSUB)
		//core symbol
		s match {
			case sl:SymbolLocalized => mi(BasicSymbols.toMathML(translate(s.name)))
			case _ => mi(BasicSymbols.toMathML(s.name))
		}
		if (s.hasSubscript) symbol(s.subscript)
		if (s.hasSuperscript) symbol(s.superscript)
		if (s.hasSubAndSupscript) end(MSUBSUP)
		else if (s.hasSuperscript) end(MSUP)
		else if (s.hasSubscript) end(MSUB)
		//sub-super script end
		if (s.hasUnderscript) symbol(s.underscript)
		if (s.hasOverscript) symbol(s.overscript)
		if (s.hasOverAndUnderscript) end(MUNDEROVER)
		else if (s.hasOverscript) end(MOVER)
		else if (s.hasUnderscript) end(MUNDER)
		//under-over script end
	}
	
	def appendStyleAttributes(style:Style) = {
		attr("mathcolor",style.hexColor)
		attr("mathbackground",style.hexBackground)
		attr("mathsize",style.font.size)
		attr("mathvariant",resolveFontVariant(style.font))
	}
	
	private def resolveStyle = if(numberStyle!=null) numberStyle else mathStyle
	
	private def resolveFontVariant(fs:FontStyle):String = {
		fs match {
			case fs if fs.italic && fs.bold => "bold-italic"
			case fs if fs.italic => "italic"
			case fs if fs.bold => "bold"
			case _ => "normal"
		}
	}
	
	def convert(s:String) = {
	    val cs = s match {
	        case "if" => {
	            locale.getLanguage match {
	                case "pl" => "dla"
	                case _ => "if"
	            }
	        }
	        case ">" => "&gt;"
	        case "<" => "&lt;"
	        case "<=" => "&le;"
	        case ">=" => "&geq;"
	        case "!=" => "&ne;"
	        case "!= 0" => "&ne; 0"
	        case "= 0" => "&equiv; 0"
	        case "=" => "="
	        case _ => s
	    }
	    mo(cs)
	}
	
}
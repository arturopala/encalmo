package org.encalmo.printer
import java.io.Writer
import org.encalmo.expression._
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
	
	var color:String = "#000000"
	var numberColor:String = null
	
	lazy val fractionFormat1:java.text.NumberFormat = new java.text.DecimalFormat(".#",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat2:java.text.NumberFormat = new java.text.DecimalFormat(".##",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat3:java.text.NumberFormat = new java.text.DecimalFormat(".###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat4:java.text.NumberFormat = new java.text.DecimalFormat(".####",java.text.DecimalFormatSymbols.getInstance(locale))
	
	override def open = {
		start(MATH)
		declareNamespace("http://www.w3.org/1998/Math/MathML")
		attr("mode","inline")
		body
		startb(MROW)
		start(MSTYLE)
		attr("scriptminsize","6pt")
		attr("scriptsizemultiplier","0.63")
		attr("linethickness","0.6")
		attr("color",color)
		body
	}
	
	override def close = {
		end(MSTYLE)
		end(MROW)
		end(MATH)
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
			startb(MROW)
		}
		if(numberColor!=null){
			start(MSTYLE)
			attr("color",numberColor)
			body
		}
		if (nf.isNegative) {
			mo("-", "prefix")
		}
		startb(MN)
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
		if(nf.hasExponent) {
			startb(MO)
			append(ENTITY_CENTER_DOT)
			end(MO)
			startb(MSUP)
			startb(MN)
			append("10")
			end(MN)
			startb(MN)
			append(nf.exponent.toString)
			end(MN)
			end(MSUP)
		}
		if(numberColor!=null){
			end(MSTYLE)
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
		mi(BasicSymbols.toMathML(s.name))
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
	
}
package org.encalmo.printer

import org.encalmo.document._
import HtmlTags._

/**
 * Html text output
 * @author artur.opala
 */
class HtmlOutput(
	val layout:Layout = Layout(),
	locale:java.util.Locale = java.util.Locale.getDefault,
	val preferences:HtmlOutputPreferences = HtmlOutputPreferences(),
	namespace:String = "",
	buffer:StringBuilder = new StringBuilder,
	indent:Indent = new Indent(2)
) 
extends XmlTextOutput(locale, namespace, buffer, indent) with LayoutBasedOutput {
	
	override def open = {
		append("<!DOCTYPE html>\r\n")
		start(HTML)
		attr("lang",locale.toString())
		body
		startb(HEADER)
		start(META)
		attr("http-equiv","Content-Type")
		attr("content","text/html; charset=utf-8")
		end
		end(HEADER)
		startb(BODY)
	}
	
	override def close = {
	    end(BODY)
		end(HTML)
		super.close
	}
	
	def toMathMLOutput:MathMLOutput = {
		new MathMLOutput(
		        locale = locale, buffer = buffer, indent = indent, namespace = null, 
		        declare = false, displayType = Some("inline"), printStyles = false
        )
	}
	
	private def resolveFontStyle(fs:FontStyle):String = if(fs.italic){"italic"}else{"normal"}
	private def resolveFontWeight(fs:FontStyle):String = if(fs.bold){"bold"}else{"normal"}
	
	def append(ch:Character) = {
		super.append(ch.text match {
			case Character.SPACE.text => "&nbsp;"
			case Character.LONGSPACE.text => "&nbsp;&nbsp;"
			case Character.LE.text => "&le;"
			case Character.GE.text => "&geq;"
			case Character.LOWER.text => "&lt;"
            case Character.GREATER.text => "&gt;"
            case Character.EQUAL.text => "="
			case Character.RARROW.text => "&rarr;"
			case _ => ch.text
		})
	}
	
	def styledef(style:Option[Style]):Unit = {
	    if(style.isDefined){
	        append(CRLF)
	        append(CSS.convertToClassDefinition(style.get))
	    }
	}
}
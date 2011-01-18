package org.encalmo.printer

import org.encalmo.document._
import XslFoTags._

/**
 * XslFo text output
 * @author artur.opala
 */
class XslFoOutput(
	val layout:Layout = Layout(),
	locale:java.util.Locale = java.util.Locale.getDefault,
	namespace:String = "fo",
	buffer:StringBuilder = new StringBuilder,
	indent:Indent = new Indent(2)
) 
extends XmlTextOutput(locale, namespace, buffer, indent) with LayoutBasedOutput {
	
	override def open = {
		super.open
		append("\r\n<!DOCTYPE ml:math PUBLIC \"-//W3C//DTD MathML 2.0//EN\" \"\">\r\n")
		start(ROOT)
		declareNamespace("http://www.w3.org/1999/XSL/Format")
		attr("font-family",DefaultFontStyle.family)
		attr("font-size",DefaultFontStyle.size,"pt")
		body
		startb(LAYOUT_MASTER_SET)
		start(SIMPLE_PAGE_MASTER)
		attr("master-name",layout.id)
		attr("page-width",layout.format.width,"mm")
		attr("page-height",layout.format.height,"mm")
		attr("margin-left",layout.leftMargin,"mm")
		attr("margin-top",layout.topMargin,"mm")
		attr("margin-right",layout.rightMargin,"mm")
		attr("margin-bottom",layout.bottomMargin,"mm")
		body
		start(REGION_BODY)
		attr("margin-top","10","mm")
		end
		start(REGION_BEFORE)
		attr("extent","10","mm")
		end
		start(REGION_AFTER)
		attr("extent","10","mm")
		end
		end(SIMPLE_PAGE_MASTER)
		end(LAYOUT_MASTER_SET)
	}
	
	override def close = {
		end("root")
		super.close
	}
	
	def toMathMLOutput:MathMLOutput = {
		new MathMLOutput(locale = locale, buffer = buffer, indent = indent)
	}
	
}
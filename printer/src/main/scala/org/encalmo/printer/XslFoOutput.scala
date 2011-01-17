package org.encalmo.printer

import org.encalmo.document._

/**
 * XslFo text output
 * @author artur.opala
 */
class XslFoOutput(val layout:Layout = Layout(), locale:java.util.Locale = java.util.Locale.getDefault) 
extends XmlTextOutput(locale) with LayoutBasedOutput {
	
	override def open = {
		super.open
		this + "<!DOCTYPE math PUBLIC \"-//W3C//DTD MathML 2.0//EN\" \"\">\r\n"
		start("fo:root")
		attr("xmlns:fo","http://www.w3.org/1999/XSL/Format")
		attr("font-family",DefaultFontStyle.family)
		attr("font-size",DefaultFontStyle.size,"pt")
		body
		startb("fo:layout-master-set")
		start("fo:simple-page-master")
		attr("master-name",layout.id)
		attr("page-width",layout.format.width,"mm")
		attr("page-height",layout.format.height,"mm")
		attr("margin-left",layout.leftMargin,"mm")
		attr("margin-top",layout.topMargin,"mm")
		attr("margin-right",layout.rightMargin,"mm")
		attr("margin-bottom",layout.bottomMargin,"mm")
		body
		start("fo:region-body")
		attr("margin-top","10","mm")
		end
		start("fo:region-before")
		attr("extent","10","mm")
		end
		start("fo:region-after")
		attr("extent","10","mm")
		end
		end("fo:simple-page-master")
		end("fo:layout-master-set")
	}
	
	override def close = {
		end("fo:root")
		super.close
	}
	
}
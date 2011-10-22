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
	val preferences:XslFoOutputPreferences = XslFoOutputPreferences(),
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
		attr("margin-top",layout.bodyTopMargin,"mm")
		attr("margin-bottom",layout.bodyBottomMargin,"mm")
		end
		start(REGION_BEFORE)
		attr("extent",layout.headerExtent,"mm")
		end
		start(REGION_AFTER)
		attr("extent",layout.footerExtent,"mm")
		end
		end(SIMPLE_PAGE_MASTER)
		end(LAYOUT_MASTER_SET)
	}
	
	override def close = {
		end(ROOT)
		super.close
	}
	
	def toMathMLOutput:MathMLOutput = {
		new MathMLOutput(locale = locale, buffer = buffer, indent = indent)
	}
	
	def appendBlockStyleAttributes(style:Style, currentStyle:Style):Unit = {
		if(style!=null){
			appendInlineStyleAttributes(style,currentStyle)
			attrNoZero("space-before",style.paragraph.spaceBefore,style.paragraph.unit)
			attrNoZero("space-after",style.paragraph.spaceAfter,style.paragraph.unit)
			attrNoZero("padding-top",style.paragraph.padding.top,style.paragraph.unit)
			attrNoZero("padding-bottom",style.paragraph.padding.bottom,style.paragraph.unit)
			attrNoZero("margin-left",style.paragraph.margin.left,style.paragraph.unit)
			attrNoZero("margin-right",style.paragraph.margin.right,style.paragraph.unit)
			attrNoZero("margin-top",style.paragraph.margin.top,style.paragraph.unit)
			attrNoZero("margin-bottom",style.paragraph.margin.bottom,style.paragraph.unit)
			attrIfChanged("text-indent",currentStyle.text.indent,style.text.indent,style.text.unit)
			appendHyphenationStyleAttributes(style,currentStyle)
		}
	}
	
	def appendListBlockStyleAttributes(style:Style):Unit = {
        attrNoZero("provisional-distance-between-starts",style.list.distanceBetweenStarts,style.list.unit)
        attrNoZero("provisional-label-separation",style.list.distanceLabelSeparation,style.list.unit)
	}
	
	def appendInlineStyleAttributes(style:Style, currentStyle:Style):Unit = {
		if(style!=null){
			attrIfChanged("font-family",currentStyle.font.family,style.font.family)
			attrIfChanged("font-size",currentStyle.font.size,style.font.size,"pt")
			attrIfChanged("font-style",resolveFontStyle(currentStyle.font),resolveFontStyle(style.font))
			attrIfChanged("font-weight",resolveFontWeight(currentStyle.font),resolveFontWeight(style.font))
			attrIfChanged("color",currentStyle.hexColor,style.hexColor)
            attrIfChanged("background-color",currentStyle.hexBackground,style.hexBackground)
			attrIfChanged("letter-spacing",currentStyle.text.letterSpacing,style.text.letterSpacing)
			attrIfChanged("word-spacing",currentStyle.text.wordSpacing,style.text.wordSpacing)
			attrIfChanged("line-height",currentStyle.text.lineHeight,style.text.lineHeight,style.text.unit)
			attrIfChanged("text-align",currentStyle.text.align,style.text.align)
			attrIfChanged("text-decoration",currentStyle.text.decoration,style.text.decoration)
			attrIfChanged("text-transform",currentStyle.text.transform,style.text.transform)
			attrNoZero("padding-left",style.paragraph.padding.left,style.paragraph.unit)
			attrNoZero("padding-right",style.paragraph.padding.right,style.paragraph.unit)
			attrNoZero("min-width",style.paragraph.width,style.paragraph.unit)
			attrNoZero("min-height",style.paragraph.height,style.paragraph.unit)
		}
	}
	
	def appendHyphenationStyleAttributes(style:Style, currentStyle:Style):Unit = {
		if(style!=null){
			if(attrIfChanged("hyphenate",currentStyle.text.hyphenate,style.text.hyphenate)){
				attrNoZero("country",locale.getCountry)
				attrNoZero("language",locale.getLanguage)
			}
		}
	}
	
	def appendTableRowStyleAttributes(style:Style, currentStyle:Style):Unit = {
		
	}
	
	def appendTableCellStyleAttributes(style:Style, currentStyle:Style):Unit = {
		
	}
	
	private def resolveFontStyle(fs:FontStyle):String = if(fs.italic){"italic"}else{"normal"}
	private def resolveFontWeight(fs:FontStyle):String = if(fs.bold){"bold"}else{"normal"}
	
	def tableColumn(width:String,unit:String) = {
		start(TABLE_COLUMN)
		attr("column-width",width,unit)
		end
	}
	
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
}
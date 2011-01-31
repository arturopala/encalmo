package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression._
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._

/**
 * Prints document as plain text 
 * @author artur.opala
 */
object PlainTextDocumentPrinter extends TextDocumentPrinter {
	
	override def print(input:Document,output:TextOutput = new TextOutput):TextOutput = {
		val t = new PlainTextDocumentPrinterTraveler(output.asWriter, output.locale)
		input.travel(traveler = t)
		output
	}

}

/**
 * Travels and prints document as plain text 
 * @author artur.opala
 */
class PlainTextDocumentPrinterTraveler(w:java.io.Writer, locale:java.util.Locale = java.util.Locale.getDefault) 
extends Traveler[DocumentComponent] {
	
	val ept = new PlainTextExpressionPrinterTraveler(w, locale)
	val dfs = java.text.DecimalFormatSymbols.getInstance(locale)
	
	val SPACE = " "
	val COMMA = dfs.getPatternSeparator
	val SEP = "\r\n"
	val TAB = "  "
		
	def write(ch:Char) = {
		w.write(ch);
		canNewLine = true
	}
		
	def write(s:String) = {
		w.write(s);
		canNewLine = true
	}
	
	def writeTabs = {
		for(x <- 1 to tabs){
			w.write(TAB)
		}
	}
	
	def writeLineEnd = {
		if(canNewLine){
			w.write(SEP);
			writeTabs
			canNewLine = false
		}
	}
	
	def writeExpression(se:Seq[ExpressionToPrint]){
		se.foreach(writeExpressionPart(_))
	}
	
	def writeExpressionPart(etp:ExpressionToPrint){
	    if(etp.prefix!=null){
	        w.write(SPACE)
	        w.write(etp.prefix)
	        w.write(SPACE)
	    }
		etp.expression.travel(traveler = ept)
		if(etp.suffix!=null){
		    w.write(SPACE)
		    w.write(etp.suffix)
		    w.write(SPACE)
		}
	}
	
	def plus = {tabs = tabs+1}
	def minus = {tabs = tabs-1}
	
	var tabs:Int = 0
	var canNewLine:Boolean = true
	
	override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case tc:TextContent => {
				if(tc.textContent!=null){
					write(tc.textContent)
				}
			}
			case _ =>
		}
		node.element match {
			case d:Document => {
				plus
				writeLineEnd
				write("")
				writeLineEnd
			}
			case c:Chapter => {
				plus
				writeLineEnd
				write("")
				writeLineEnd
			}
			case s:Section => {
				plus
				writeLineEnd
			}
			case expr:InlineExpr => {
				val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
				ess.foreach(e => {write(" ");writeExpression(e);write(" ")})
			}
			case expr:BlockExpr => {
				val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
				plus
			    ess.foreach(e => {
			        canNewLine = true;
			        writeLineEnd;
                    if(e.head.expression.isInstanceOf[SymbolWithDescription]){
                        write(e.head.expression.asInstanceOf[SymbolWithDescription].description)
                        plus
                        canNewLine = true;
                        writeLineEnd;
                    }
			        writeExpression(e)
                    minus
		        })
			    canNewLine = true
			    writeLineEnd
			    minus
			}
			case _ =>
		}
	}
	
	override def onBeforeChildEnter(node:Node[DocumentComponent], position:Int, child:DocumentComponent):Unit = Unit
	
	override def onBetweenChildren(node:Node[DocumentComponent], leftChild:DocumentComponent, rightChild:DocumentComponent):Unit = {
		(leftChild,rightChild) match {
			case (tl:Text,tr:Text) => {
				write(" ")
			}
			case _ =>
		}
	}
	
	override def onAfterChildExit(node:Node[DocumentComponent], position:Int, child:DocumentComponent):Unit = Unit
	
	override def onExit(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case d:Document => {
				minus
			}
			case c:Chapter => {
				minus
			}
			case s:Section => {
				minus
			}
			case _ =>
		}
	}
	
}


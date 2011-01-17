package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression._
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._

/**
 * Prints document as xsl-fo text 
 * @author artur.opala
 */
object XslFoTextDocumentPrinter extends DocumentPrinter[XslFoOutput,String] {
	
	override def print(input:Document,output:XslFoOutput = new XslFoOutput):XslFoOutput = {
		val t = new XslFoTextDocumentPrinterTraveler(output, output.locale)
		input.travel(traveler = t)
		output
	}

}

/**
 * Travels and prints document as xsl-fo text 
 * @author artur.opala
 */
class XslFoTextDocumentPrinterTraveler(output:XslFoOutput, locale:java.util.Locale = java.util.Locale.getDefault) 
extends Traveler[DocumentComponent] {
	
	val w = output.asWriter
	val ept = new MathMLExpressionPrinterTraveler(w, locale)
	val dfs = java.text.DecimalFormatSymbols.getInstance(locale)
	
	val SPACE = " "
	val COMMA = dfs.getPatternSeparator
		
	def write(ch:Char) = {
		w.write(ch);
	}
		
	def write(s:String) = {
		w.write(s);
	}
	
	def writeExpressionSeq(se:Seq[Expression]){
		if(se.head!=null){
			writeExpression(se.head)
		}
		if(se.tail!=null && !se.tail.isEmpty){
			se.tail.foreach(e => {
				write(" = ")
				writeExpression(e)
			})
		}
	}
	
	def writeExpression(e:Expression){
		e.travel(traveler = ept)
	}
	
	var isInFlow:Boolean = false
	
	def tryStartPageSequence(chapter:Chapter) = {
		if(!isInFlow){
			output.start("fo:page-sequence")
			output.attr("master-reference", output.layout.id)
			output.body
			if(chapter!=null){
				if(chapter.header!=null){
					output.start("fo:static-content")
					output.attr("flow-name","xsl-region-before")
					output.body
					chapter.header.travel(traveler=this)
					output.end("fo:static-content")
				}
				if(chapter.footer!=null){
					output.start("fo:static-content")
					output.attr("flow-name","xsl-region-after")
					output.body
					chapter.footer.travel(traveler=this)
					output.end("fo:static-content")
				}
			}
			output.start("fo:flow")
			output.attr("flow-name","xsl-region-body")
			output.body
			isInFlow = true
		}
	}
	
	def endPageSequence = {
		if(!isInFlow){
			output.end("fo:flow")
			output.end("fo:page-sequence")
			isInFlow = false
		}
	}
	
	override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case d:Document => {
				
			}
			case chapter:Chapter => {
				tryStartPageSequence(chapter)
			}
			case _ => {
				tryStartPageSequence(null)
			}
		}
		node.element match {
			case s:Section => {
				
			}
			case expr:Expr => {
				val ess:Seq[Seq[Expression]] = expr.resolve
				if(ess.head!=null){
					writeExpressionSeq(ess.head)
				}
				if(ess.tail!=null && !ess.tail.isEmpty){
					ess.tail.foreach(es => {
						write(COMMA)
						write(SPACE)
						writeExpressionSeq(es)
					})
				}
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
				
			}
			case c:Chapter => {
				
			}
			case s:Section => {
				
			}
			case _ =>
		}
	}
	
}


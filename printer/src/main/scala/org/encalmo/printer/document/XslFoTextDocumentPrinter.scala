package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression._
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._
import XslFoTags._

/**
 * Prints document as xsl-fo text 
 * @author artur.opala
 */
object XslFoTextDocumentPrinter extends DocumentPrinter[XslFoOutput,String] {
	
	override def print(input:Document,output:XslFoOutput = new XslFoOutput):XslFoOutput = {
		val t = new XslFoTextDocumentPrinterTraveler(output)
		input.travel(traveler = t)
		output
	}

}

/**
 * Travels and prints document as xsl-fo text 
 * @author artur.opala
 */
class XslFoTextDocumentPrinterTraveler(output:XslFoOutput) 
extends Traveler[DocumentComponent] {
	
	val w = output.asWriter
	val locale = output.locale
	val mathOutput = output.toMathMLOutput
	val ept = new MathMLExpressionPrinterTraveler(mathOutput)
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
				output.append(" = ")
				writeExpression(e)
			})
		}
		if(se.tail!=null && !se.tail.isEmpty){
			se.tail.foreach(e => {
				output.append(" = ")
				writeExpression(e)
			})
		}
	}
	
	def writeExpression(e:Expression){
		output.startb(INSTREAM_FOREIGN_OBJECT)
		mathOutput.open
		e.travel(traveler = ept)
		mathOutput.close
		output.end(INSTREAM_FOREIGN_OBJECT)
	}
	
	var isInFlow:Boolean = false
	
	def tryStartPageSequence(chapter:Chapter) = {
		if(!isInFlow || chapter!=null){
			if(isInFlow || chapter!=null){
				endPageSequence
			}
			output.start(PAGE_SEQUENCE)
			output.attr("master-reference", output.layout.id)
			output.body
			if(chapter!=null){
				if(chapter.header!=null){
					output.start(STATIC_CONTENT)
					output.attr("flow-name","xsl-region-before")
					output.body
					isInFlow = true
					chapter.header.travel(traveler=this)
					isInFlow = false
					output.end(STATIC_CONTENT)
				}
				if(chapter.footer!=null){
					output.start(STATIC_CONTENT)
					output.attr("flow-name","xsl-region-after")
					output.body
					isInFlow = true
					chapter.footer.travel(traveler=this)
					isInFlow = false
					output.end(STATIC_CONTENT)
				}
			}
			output.start(FLOW)
			output.attr("flow-name","xsl-region-body")
			output.body
			isInFlow = true
		}
	}
	
	def endPageSequence = {
		if(isInFlow){
			output.end(FLOW)
			output.end(PAGE_SEQUENCE)
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
				output.startb(BLOCK)
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
				endPageSequence
			}
			case c:Chapter => {
				endPageSequence
			}
			case s:Section => {
				output.end(BLOCK)
			}
			case _ =>
		}
	}
	
}


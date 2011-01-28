package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression._
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._
import XslFoTags._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Stack
import scala.collection._

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
	
	/** Section counters map */
	private val counterMap:LinkedHashMap[Enumerator,SectionCounter] = LinkedHashMap[Enumerator,SectionCounter]()
	
	private val styleStack:Stack[Style] = Stack()
	
	/** Returns counter linked to the enumerator */
	private def counterFor(en:Enumerator):SectionCounter = {
		var sco = counterMap.get(en)
		if(!sco.isDefined){
			sco = Some(SectionCounter(en))
			counterMap.put(en,sco.get)
		}
		sco.get
	}
	
	def write(ch:Char) = {
		w.write(ch);
	}
		
	def write(s:String) = {
		w.write(s);
	}
	
	def writeWithSpaceAround(s:String) = {
		if(s!=null){
			output.start(INLINE)
			output.attr("padding-start","0.1em")
			output.attr("padding-end","0.1em")
			output.body
			output.append(s)
			output.end(INLINE)
		}
	}
	
	def writeExpressionSeq(se:Seq[ExpressionToPrint], style:Style, printDescription:Boolean){
	    if(!se.isEmpty){
	        if(se.head!=null){
                writeExpressionSeqStart(se.head, style, printDescription)
	        }
    	    output.start(LIST_ITEM_BODY)
    	    output.attr("start-indent","body-start()")
    	    output.body
    	    output.startb(BLOCK)
    		if(se.head!=null){
    			writeExpression(se.head, style)
    		}
    		if(se.tail!=null && !se.tail.isEmpty){
    			se.tail.foreach(etp => {
    				writeExpression(etp, style)
    			})
    		}
    		output.end(BLOCK)
            output.end(LIST_ITEM_BODY)
	    }
	}
	
	def writeExpressionSeqStart(etp:ExpressionToPrint, style:Style, printDescription:Boolean) = {
	    output.start(LIST_ITEM_LABEL)
	    output.attr("end-indent","label-end()")
	    val descStyle = etp.stylesConfig match {
            case Some(x) => x.expressions.symbolDescription.getOrElse(styleStack.top)
            case None => styleStack.top
        }
        output.appendInlineStyleAttributes(descStyle,styleStack.top)
        output.body
        output.start(BLOCK)
        output.body
        output.append(style.list.bullet)
        if(printDescription){
    	    etp.expression match {
                case symb:SymbolWithDescription => {
                    if(symb.description!=null){
                        output.startb(INLINE)
                        output.append(symb.description)
                        output.end(INLINE)
                    }
                }
                case _ => 
            }
        }
        output.end(BLOCK)
        output.end(LIST_ITEM_LABEL)
	}
	
	def writeExpression(etp:ExpressionToPrint, style:Style) = {
		val mc = mathOutput.mathStyle
		writeWithSpaceAround(etp.prefix)
		output.start(INSTREAM_FOREIGN_OBJECT)
        if (etp.style!=null){
            output.appendInlineStyleAttributes(etp.style, if(style!=null)style else styleStack.top)
            mathOutput.mathStyle = etp.style
        }else{
            if(style!=null) {
                output.appendInlineStyleAttributes(style, styleStack.top)
                mathOutput.mathStyle = style
            }
        }
        output.body
        mathOutput.open
        etp.expression.travel(traveler = ept)
        mathOutput.close
        mathOutput.mathStyle = mc
        output.end(INSTREAM_FOREIGN_OBJECT)
		writeWithSpaceAround(etp.suffix)
	}
	
	var isInFlow:Boolean = false
	
	def tryStartPageSequence(chapter:Chapter, style:Style) = {
		if(!isInFlow || chapter!=null){
			if(isInFlow || chapter!=null){
				endPageSequence
			}
			output.start(PAGE_SEQUENCE)
			output.attr("master-reference", output.layout.id)
			output.appendBlockStyleAttributes(style, styleStack.top)
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
			output.startb(BLOCK)
			isInFlow = true
		}
	}
	
	def endPageSequence = {
		if(isInFlow){
			output.end(BLOCK)
			output.end(FLOW)
			output.end(PAGE_SEQUENCE)
			isInFlow = false
		}
	}
	
	override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case sc:StylesConfig => {
				sc.expressions.numbers match {
					case Some(s) => {mathOutput.numberStyle = s}
					case None => Unit
				}
				return
			}
			case nvc:NonVisualDocumentComponent => return
			case d:Document => {
				
			}
			case chapter:Chapter => {
				tryStartPageSequence(chapter,chapter.style)
			}
			case _ => {
				tryStartPageSequence(null,node.element.style)
				node.element match {
					case ns:NumSection => {
						val en:Enumerator = ns.enumerator
						val sc = counterFor(en)
						output.start(BLOCK)
						output.appendBlockStyleAttributes(ns.style,styleStack.top)
						output.body
						val ens = en.style
						if(ens!=null){
							output.start(INLINE)
							output.appendInlineStyleAttributes(en.style, styleStack.top)
							output.body
						}
						
						output.append(sc.current.mkString("",".","."+SPACE))
						sc.in // counter level increment
						if(ens!=null){
							output.end(INLINE)
						}
					}
					case s:Section => {
						output.start(BLOCK)
						output.appendBlockStyleAttributes(s.style, styleStack.top)
						output.body
					}
					case t:Text => {
						if(t.myStyle!=null){
							output.start(INLINE)
							output.appendInlineStyleAttributes(t.myStyle, styleStack.top)
							output.body
						}
						output.append(t.text);
						if(t.myStyle!=null){
							output.end(INLINE)
						}
					}
					case expr:Expr => {
						val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
						if(!ess.isEmpty){
						    val doPrintDescription:Boolean = expr.isPrintDescription && ess.forall(es => es.headOption.exists(ept => ept.expression.isInstanceOf[SymbolWithDescription]))
							if(expr.isForceLineBreak){
							    output.start(LIST_BLOCK)
							    if(expr.isPrintDescription){
							        output.appendListBlockStyleAttributes(expr.style)
							    }else{
							        output.attr("provisional-distance-between-starts","1em")
						            output.attr("provisional-label-separation","0.2em")
							    }
                                if(expr.myStyle!=null){
                                    output.appendBlockStyleAttributes(expr.myStyle, styleStack.top)
                                }
                                output.body
								for(es <- ess){
									output.start(LIST_ITEM)
									output.body
									writeExpressionSeq(es, expr.style, doPrintDescription)
									output.end(LIST_ITEM)
								}
                                output.end(LIST_BLOCK)
							}else{
								if(expr.myStyle!=null){
									output.start(INLINE)
									output.appendInlineStyleAttributes(expr.myStyle, styleStack.top)
									output.attr("padding-end","1em")
									output.body
								}
								if(ess.head!=null){
									writeExpressionSeq(ess.head, expr.style, false)
								}
								if(ess.tail!=null && !ess.tail.isEmpty){
									ess.tail.foreach(es => {
										writeExpressionSeq(es, expr.style, false)
									})
								}
								if(expr.myStyle!=null){
									output.end(INLINE)
								}
							}
						}
					}
					case _ => {}
				}
			}
		}
		// push current style on the stack
		styleStack.push(node.element.style)
	}
	
	override def onBeforeChildEnter(node:Node[DocumentComponent], position:Int, child:DocumentComponent):Unit = Unit
	
	override def onBetweenChildren(node:Node[DocumentComponent], leftChild:DocumentComponent, rightChild:DocumentComponent):Unit = {
		(leftChild,rightChild) match {
			case (tl:Text,tr:Text) => {
				write(SPACE)
			}
			case _ =>
		}
	}
	
	override def onAfterChildExit(node:Node[DocumentComponent], position:Int, child:DocumentComponent):Unit = Unit
	
	override def onExit(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case nvc:NonVisualDocumentComponent => return
			case _ => 
		}
		// removing current style from the stack
		styleStack.pop
		node.element match {
			case d:Document => {
				endPageSequence
			}
			case c:Chapter => {
				endPageSequence
			}
			case ns:NumSection => {
				output.end(BLOCK)
				val sc = counterFor(ns.enumerator)
				sc.out //counter level decrement
				sc.next // counter increment
			}
			case s:Section => {
				output.end(BLOCK)
			}
			case _ =>
		}
	}
	
}


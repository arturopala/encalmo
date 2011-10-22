package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression.SymbolLike
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._
import org.encalmo.printer.HtmlTags._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Stack
import scala.collection._

/**
 * Prints document as html5 text 
 * @author artur.opala
 */
object HtmlTextDocumentPrinter extends DocumentPrinter[HtmlOutput,String] {
	
	override def print(input:Document,output:HtmlOutput = new HtmlOutput):HtmlOutput = {
		val t = new HtmlTextDocumentPrinterTraveler(output)
		input.travel(traveler = t)
		output
	}

}

/** 
 * Travels and prints document as html5 text 
 * @author artur.opala
 */
class HtmlTextDocumentPrinterTraveler(output:HtmlOutput) 
extends Traveler[DocumentComponent] {
	
	val w = output.asWriter
	val locale = output.locale
	val mathOutput = output.toMathMLOutput
	val ept = new MathMLExpressionPrinterTraveler(mathOutput)
	val dfs = java.text.DecimalFormatSymbols.getInstance(locale)
	
	val SPACE = " "
	val COMMA = dfs.getPatternSeparator
	
	val customStyles = """ 
	    body {font-size:12pt}
		.et  {width:100%}
        .ec1 {width:2%; margin-right:0.7em; text-align:right; vertical-align:top}
	    .ec2 {width:28%; margin-right:3pt; text-align:left; vertical-align:top}
	    .ec3 {width:5%; font-size:12pt; font-weight: bold}
	    .ec4 {width:65%; font-size:10pt}
	    .usc {border-bottom:0.4pt dotted black;}
	"""
	    
	val blockExprPrintStrategy:ExpressionPrintStrategy = output.preferences.expressionPrintStrategy match {
		case "table" => new ExpressionPrintAsTableStrategy(this)
		case _ => new ExpressionPrintAsTableStrategy(this)
	}
	
	/** Section counters map */
	private val counterMap:LinkedHashMap[Enumerator,SectionCounter] = LinkedHashMap[Enumerator,SectionCounter]()
	
	private val styleStack:Stack[Style] = Stack()
	styleStack.push(DefaultStyle)
	
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
	
	override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case sc:StylesConfig => {
				sc.expressions.numbers match {
					case Some(s) => {mathOutput.numberStyle = s}
					case None => Unit
				}
				output.startb(STYLE)
				sc.all.foreach(output.styledef(_))
				output.end(STYLE)
				return
			}
			case nvc:NonVisualComponent => return
			case d:Document => {
				output.startb(STYLE)
				output.append(customStyles)
				output.end(STYLE)
				return
			}
			case chapter:Chapter => {
			    output.startb(P)
				chapter.header.travel(traveler=this)
			}
			case _ => {
				node.element match {
					case ns:NumSection => {
						val en:Enumerator = ns.enumerator
						val sc = counterFor(en)
						output.startb(DIV,ns.style.classId)
						val ens = en.style
						if(ens!=null){
							output.startb(SPAN,en.style.classId)
						}
						output.append(sc.current.mkString("",".","."+SPACE))
						sc.in // counter level increment
						if(ens!=null){
							output.end(SPAN)
						}
					}
					case s:Section => {
						output.startb(DIV, s.style.classId)
					}
					case ch:Character => {
						output.append(ch)
					}
					case ttt:TextToTranslate => {
						output.append(Translator.translate(ttt.text,locale,ttt.dictionary).getOrElse(ttt.text))
					}
					case t:TextContent => {
						if(t.myStyle!=null){
							output.startb(SPAN,t.myStyle.classId)
						}
						output.append(t.textContent);
						if(t.myStyle!=null){
							output.end(SPAN)
						}
					}
					case expr:InlineExpr => {
						val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
						ess.foreach(es => {
							if(expr.myStyle!=null){
								output.start(SPAN,expr.myStyle.classId)
								output.attr("style","padding-end:1em")
								output.body
							}
							es.foreach(etp => {
			                    writeExpression(etp, expr.style)
			                })
							if(expr.myStyle!=null){
								output.end(SPAN)
							}
						})
					}
					case expr:BlockExpr => {
						val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
						if(!ess.isEmpty){
							blockExprPrintStrategy.print(node,expr,ess)
						}
					}
					case a:Assertion => {
						val result = a.evaluate
						val s = Section(a.style,result._2:_*)
						s.travel(traveler = this);
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
			case nvc:NonVisualComponent => return
			case _ => 
		}
		// removing current style from the stack
		styleStack.pop
		node.element match {
			case d:Document => {
				
			}
			case c:Chapter => {
				output.end(P)
			}
			case ns:NumSection => {
				output.end(DIV)
				val sc = counterFor(ns.enumerator)
				sc.out //counter level decrement
				sc.next // counter increment
			}
			case s:Section => {
				output.end(DIV)
			}
			case _ =>
		}
	}
	
	/** Expression print strategy */
	trait ExpressionPrintStrategy {
		def print(node:Node[DocumentComponent],expr:BlockExpr,ess:Seq[Seq[ExpressionToPrint]])
    }
    
    /** Print expression as table */
    class ExpressionPrintAsTableStrategy (
		traveler:HtmlTextDocumentPrinterTraveler
	)extends ExpressionPrintStrategy {
    	
    	override def print(node:Node[DocumentComponent],expr:BlockExpr,ess:Seq[Seq[ExpressionToPrint]]) = {
    		val parentNumSection = expr.parentOfType[NumSection](classOf[NumSection])
    		val styleConfigOpt = expr.parentStylesConfig 
    		val sc:Option[SectionCounter] = parentNumSection.map(_.enumerator).map(counterFor(_))
			val tableRowStyle:Option[Style] = styleConfigOpt match {
				case Some(styleConfig) => styleConfig.expressions.block 
				case None => None
			}
    		output.start(TABLE,"et")
    		if(expr.isFirstBlockComponent){
			    parentNumSection.map(x => output.attr("style","space-before:",x.style.paragraph.spaceBefore*0.8))
			}
    		output.body
			for(es <- ess){
				output.startb(TR)
				val bullet = sc.map(_.currentCounter.item+")").getOrElse(null)
				writeExpressionSeq(es, expr.style, expr.isPrintDescription, bullet, tableRowStyle/*, false*/)
				sc.foreach(_.next)
				output.end(TR)
			}
            output.end(TABLE)
    	}
    	
    	def writeExpressionSeq(se:Seq[ExpressionToPrint], style:Style, printDescription:Boolean, bullet:String, tableRowStyle:Option[Style]/*, secondTableRow:Boolean*/){
		    if(!se.isEmpty){
	        	val etp1 = se.head
	        	val description:Option[String] = etp1.expression match {
	        		case s:SymbolLike => s.symbol.localizedDescription(locale)
	        		case _ => None
	        	}
	        	val printable:Boolean = etp1.expression.printable
	        	val isPrintDescription = printDescription && description.isDefined
	        	val paddingTop = tableRowStyle match {
	        		case Some(x) => x.paragraph.spaceBefore 
	        		case None => 3
	        	}
	        	val paddingBottom = tableRowStyle match {
	        		case Some(x) => x.paragraph.spaceAfter
	        		case None => 3
	        	}
	        	val indent:Int = if(etp1.style!=null && etp1.style.paragraph.width>0) etp1.style.paragraph.width else 30
		    	val isCell1 = bullet!=null
		        val isCell2 = isPrintDescription
                val descStyle = etp1.stylesConfig match {
		            case Some(x) => x.expressions.symbolDescription.getOrElse(styleStack.top)
		            case None => styleStack.top
		        }
		    	if(isCell1){
					output.startb(TD,"ec1")
					output.startb(SPAN,descStyle.classId)
			        output.append(bullet)
			        output.end(SPAN)
			        output.end(TD)
		    	}
		        if(isCell2){
			        output.startb(TD,"ec2 usc")
			        output.startb(SPAN,descStyle.classId)
		        	output.append(description)
		        	output.end(SPAN)
			        output.end(TD)
		    	}
		        etp1.expression match {
	        		case s:SymbolLike => {
	        		    output.startb(TD,"ec3 usc")
	        		    writeExpression(etp1, style)
	        		    output.end(TD)
	        		    output.start(TD,"ec4 usc")
				        if(!isCell2){
				        	val ncs:Int = 2 + {if(isCell1) 0 else 1}
				        	output.attr("colspan",ncs);
				        }
				        output.body
				        se.tail.foreach(etp => {
		    				writeExpression(etp, style)
		    			})
				        output.end(TD)
	        		}
	        		case _ => {
	        		    output.start(TD,"ec4 usc")
				        if(!isCell2){
				        	val ncs:Int = 2 + {if(isCell1) 0 else 1}
				        	output.attr("colspan",ncs);
				        }
				        output.body
				        se.foreach(etp => {
		    				writeExpression(etp, style)
		    			})
				        output.end(TD)
	        		}
	        	}
		        
		    }
		}
    	
	}
   
	def writeExpression(etp:ExpressionToPrint, style:Style):Unit = {
		if(etp.expression.printable){
			val mc = mathOutput.mathStyle
			if(etp.prefix!=null)mathOutput.prefix = etp.prefix
			if(etp.suffix!=null)mathOutput.suffix = etp.suffix
	        if (etp.style!=null){
	            mathOutput.mathStyle = etp.style
	        }else{
	            if(style!=null) {
	                mathOutput.mathStyle = style
	            }
	        }
			output.start(SPAN, mathOutput.mathStyle.classId)
	        output.body
	        mathOutput.open
	        etp.expression.travel(traveler = ept)
	        mathOutput.close
	        mathOutput.mathStyle = mc
	        output.end(SPAN)
		}
	}
	
}




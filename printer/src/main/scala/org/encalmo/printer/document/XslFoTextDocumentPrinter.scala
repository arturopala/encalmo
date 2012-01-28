package org.encalmo.printer.document

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Stack
import scala.collection._
import org.encalmo.common._
import org.encalmo.document._
import org.encalmo.expression.SymbolLike
import org.encalmo.printer.XslFoTags._
import org.encalmo.printer.expression._
import org.encalmo.printer._
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle
import org.encalmo.expression.MultipleInfixOperation
import org.encalmo.expression.Diff
import org.encalmo.expression.Transparent
import org.encalmo.expression.Expression

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
    val THINSPACE = "&thinsp;"
	val COMMA = dfs.getPatternSeparator
	
	val blockExprPrintStrategy:ExpressionPrintStrategy = output.preferences.expressionPrintStrategy match {
		case "table" => new ExpressionPrintAsTableStrategy(this)
		case _ => new ExpressionPrintAsTableStrategy(this)
	}
	
	/** Section counters map */
	private val counterMap:LinkedHashMap[Enumerator,SectionCounter] = LinkedHashMap[Enumerator,SectionCounter]()
	
	private val styleStack:Stack[Style] = Stack()
	styleStack.push(DefaultStyle)
	
	/** Returns counter linked to the enumerator */
	protected def counterFor(en:Enumerator):SectionCounter = {
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
	
	var isInFlow:Boolean = false
	
	def tryStartPageSequence(chapter:Chapter, style:Style):Unit = {
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
					output.start(BLOCK)
                    output.attr("text-align","center")
                    output.attr("font-size","80%")
                    output.attr("line-height","125%")
                    output.attr("padding-bottom","0.5em")
                    output.attr("border-bottom","0.3pt solid black")
                    output.body
					isInFlow = true
					chapter.header.travel(traveler=this)
					isInFlow = false
					output.end(BLOCK)
					output.end(STATIC_CONTENT)
				}
				if(chapter.footer!=null){
					output.start(STATIC_CONTENT)
					output.attr("flow-name","xsl-region-after")
					output.body
                    output.start(BLOCK)
                    output.attr("text-align","right")
                    output.attr("border-top","1pt solid black")
                    output.body
					isInFlow = true
					chapter.footer.travel(traveler=this)
					isInFlow = false
					output.startb(INLINE)
					output.elem(PAGE_NUMBER)
					output.end(INLINE)
					output.end(BLOCK)
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
			case nvc:NonVisualComponent => return
			case d:Document => {
			    d.stylesConfig.numbers match {
					case Some(s) => {mathOutput.numberStyle = s}
					case None => Unit
				}
				return
			}
			case chapter:Chapter => {
				tryStartPageSequence(chapter,chapter.style)
			}
            case toc:TableOfContents => {
                output.start(BLOCK)
                output.attr("margin-top","1em")
                output.attr("margin-bottom","1em")
                output.body
                if(toc.parentDocument.isDefined){
                    output.start(BLOCK)
                    output.attr("margin-bottom","0.3em")
                    output.attr("font-size", "12pt")
                    output.body
                    output.append(toc.title)
                    output.end(BLOCK)
                    toc.parentDocument.get.travel(traveler = new XslFoTableOfContentsPrinterTraveler(output))
                }
                output.end(BLOCK)
            }
            case PageBreak => {
                output.start(BLOCK)
                output.attr("break-after","page")
                output.end
            }
			case _ => {
				tryStartPageSequence(null,node.element.style)
				node.element match {
					case ns:NumSection => {
						val en:Enumerator = ns.enumerator
						val sc = counterFor(en)
						output.start(BLOCK)
						output.appendBlockStyleAttributes(ns.style,styleStack.top)
						if(ns.isFirstBlockComponent){
						    output.attr("keep-with-previous","always")
						}
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
						if(ns.title.isDefined){
							output.start(INLINE)
							output.appendInlineStyleAttributes(ns.style, styleStack.top)
							output.body
							output.append(ns.title.get);
							output.end(INLINE)
						}
					}
					case s:Section => {
						output.start(BLOCK)
						output.appendBlockStyleAttributes(s.style, styleStack.top)
						output.body
					}
					case ch:Character => {
						output.append(ch)
					}
					case ttt:TextToTranslate => {
						output.append(Translator.translate(ttt.text,locale,ttt.dictionary).getOrElse(ttt.text))
					}
					case t:TextContent => {
						if(t.myStyle!=null){
							output.start(INLINE)
							output.appendInlineStyleAttributes(t.myStyle, styleStack.top)
							output.body
						}
						output.append(t.textContent);
						if(t.myStyle!=null){
							output.end(INLINE)
						}
					}
					case expr:InlineExpr => {
						val ess:Seq[Seq[ExpressionToPrint]] = expr.resolve
						ess.foreach(es => {
							if(expr.myStyle!=null){
								output.start(INLINE)
								output.appendInlineStyleAttributes(expr.myStyle, styleStack.top)
								output.attr("padding-end","1em")
								output.body
							}
							es.foreach(etp => {
			                    writeExpression(etp, expr.style)
			                })
							if(expr.myStyle!=null){
								output.end(INLINE)
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
		styleStack.push(node.element.style.withoutParagraphStyle)
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
	
	/** Expression print strategy */
	trait ExpressionPrintStrategy {
		def print(node:Node[DocumentComponent],expr:BlockExpr,ess:Seq[Seq[ExpressionToPrint]])
    }
    
    /** Print expression as table */
    class ExpressionPrintAsTableStrategy (
		traveler:XslFoTextDocumentPrinterTraveler
	)extends ExpressionPrintStrategy {
    	
    	override def print(node:Node[DocumentComponent],expr:BlockExpr,ess:Seq[Seq[ExpressionToPrint]]) = {
    		val parentNumSection = expr.parentOfType[NumSection](classOf[NumSection])
    		val styleConfigOpt = expr.parentStylesConfig 
    		val sc:Option[SectionCounter] = parentNumSection.map(_.enumerator).map(counterFor(_))
			val tableRowStyle:Option[Style] = styleConfigOpt match {
				case Some(styleConfig) => styleConfig.block 
				case None => None
			}
    		
    		output.start(TABLE)
    		output.attr("table-layout","fixed")
    		output.attr("width","100%")
            //output.attr("border-collapse","collapse")
    		if(expr.isFirstBlockComponent){
    			//output.attr("keep-with-previous","always")
			    parentNumSection.map(x => output.attr("space-before",x.style.paragraph.spaceBefore*0.8))
			}
    		output.body
    		output.tableColumn("2.5","em")
    		output.tableColumn("proportional-column-width(40)","")
    		output.tableColumn("proportional-column-width(60)","")
    		output.start(TABLE_BODY)
			output.body
			for(es <- ess){
				output.startb(TABLE_ROW)
				val bullet = sc.map(_.currentCounter.item+")").getOrElse(null)
				writeExpressionSeq(es, expr.style, expr.isPrintDescription, bullet, tableRowStyle, false)
				sc.foreach(_.next)
				output.end(TABLE_ROW)
			}
            output.end(TABLE_BODY)
            output.end(TABLE)
    	}
    	
    	def writeExpressionSeq(se:Seq[ExpressionToPrint], style:Style, printDescription:Boolean, bullet:String, tableRowStyle:Option[Style], secondTableRow:Boolean){
		    if(!se.isEmpty){
	        	val etp1 = se.head
	        	val description:Option[String] = etp1.expression match {
	        		case s:SymbolLike => s.symbol.localizedDescription(locale)
	        		case _ => None
	        	}
	        	val printable:Boolean = etp1.expression.printable
	        	val isPrintDescription = printDescription && description.isDefined && description.get!=""
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
		            case Some(x) => x.symbolDescription.getOrElse(styleStack.top)
		            case None => styleStack.top
		        }
	        	val leafs:Int = se.map(x => x.expression.countTreeLeafs).sum
	        	val twoRows:Boolean = !secondTableRow && (leafs>15 || (description match {
	        		case Some(d) => d.size>150
	        		case None => false
	        	}))
	        	val twoTableRows = !secondTableRow && (isCell2 && twoRows && leafs<15)
		    	if(isCell1){
					output.start(TABLE_CELL)
                    output.attr("border-bottom","0.13mm solid black")
                    output.attr("border-top","0.13mm solid black")
                    output.attr("border-left","0.13mm solid black")
					output.body
			        output.start(BLOCK)
			        output.attr("padding-top",paddingTop,"pt")
			        output.attr("padding-bottom",paddingBottom,"pt")
			        output.attr("margin-right","0.7em")
			        output.attr("text-align","right")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.body
			        output.append(bullet)
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
		    	}
		        if(isCell2 && !twoRows){
			        output.start(TABLE_CELL)
			        if(!twoTableRows) {
			            output.attr("border-bottom","0.13mm solid black")
			            output.attr("border-top","0.13mm solid black")
			        }
			        output.body
			        output.start(BLOCK)
			        output.attr("margin-right","3pt")
			        output.attr("padding-top",paddingTop,"pt")
			        output.attr("padding-bottom",paddingBottom,"pt")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.appendHyphenationStyleAttributes(descStyle,styleStack.top)
		        	output.attr("keep-together.within-page","always")
			        output.body
			        if(!secondTableRow) {
			        	output.append(description)
			        }
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
		    	}
		        output.start(TABLE_CELL)
		        if(!secondTableRow && (!isCell2 || twoRows)){
		        	val ncs:Int = 2 + {if(isCell1) 0 else 1}
		        	output.attr("number-columns-spanned",ncs);
		        }
				if(!twoTableRows){
				    output.attr("border-bottom","0.13mm solid black")
                    output.attr("border-top","0.13mm solid black")
                    output.attr("border-right","0.13mm solid black")
				}
			    output.attr("vertical-align","middle")
		        output.body
		        if(isCell2 && twoRows){
		        	output.start(BLOCK)
		        	output.attr("keep-together.within-page","always")
			        output.attr("margin-top",paddingTop,"pt")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.body
			        output.append(description)
			        output.end(BLOCK)
		        }
				if(twoTableRows){
					output.end(TABLE_CELL)
					output.end(TABLE_ROW)
					output.start(TABLE_ROW)
					output.attr("keep-with-previous","always")
					output.body
					writeExpressionSeq(se, style, printDescription, "", tableRowStyle, true)
				}else{
			        output.start(BLOCK)
			        output.appendInlineStyleAttributes(style,styleStack.top)
			        if(printable){
			        	output.attr("text-indent",-indent-3,"pt")
			        }
			        output.attr("margin-top",paddingTop,"pt")
			        output.attr("margin-bottom",paddingBottom,"pt")
			        output.attr("margin-left",indent+5,"pt")
			        output.attr("keep-together.within-page","always")
			        output.body
			        se.foreach(etp => {
	    				writeExpression(etp, style)
	    			})
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
				}
		    }
		}
    	
	}
        
    def writeExpression(etp:ExpressionToPrint, style:Style, parentNode:Node[Expression] = null, position:Int=0, span:Boolean = true):Unit = {
        if(etp.expression.printable){
            etp.expression match {
                case tr:Transparent => {
                    val node = Node[Expression](parentNode,tr,position)
                    tr.children.size match {
                        case 0 => Unit
                        case 1 => writeExpression(ExpressionToPrint(tr.children.head,etp.style,etp.prefix,etp.suffix,etp.stylesConfig),style,node,0,span)
                        case _ => {
                            writeExpression(ExpressionToPrint(tr.children.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
                            tr.children.tail.foreach(e => {
                                writeExpression(ExpressionToPrint(e,etp.style,null,null,etp.stylesConfig),style,node,1,false)
                            })
                            if(etp.suffix!=null)output.append(etp.suffix)
                        }
                    }
                }
                case mio:MultipleInfixOperation if mio.args.size > 1 => {
                    val node = Node[Expression](parentNode,mio,position)
                    writeExpression(ExpressionToPrint(mio.args.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
                    mio.args.tail.foreach(e => {
                        writeExpression(ExpressionToPrint(e,etp.style,output.convertOperator(mio.operator),null,etp.stylesConfig),style,node,1,false)
                    })
                    if(etp.suffix!=null)output.append(etp.suffix)
                }
                case _ => {
                    val mc = mathOutput.mathStyle
                    output.startNoIndent(INSTREAM_FOREIGN_OBJECT)
                    if (etp.style!=null){
                        output.attrNoZero("min-width",etp.style.paragraph.width,etp.style.paragraph.unit)
                        mathOutput.mathStyle = etp.style
                    }else{
                        if(style!=null) {
                            output.attrNoZero("min-width",style.paragraph.width,style.paragraph.unit)
                            mathOutput.mathStyle = style
                        }
                    }
                    output.body
                    mathOutput.open
                    if(etp.prefix!=null && etp.prefix!="") mathOutput.mo(etp.prefix,MathMLTags.INFIX,MathMLTags.THICKMATHSPACE,MathMLTags.THICKMATHSPACE)
                    etp.expression.travel(traveler = ept, parentNode = parentNode, position = position)
                    if(etp.suffix!=null && etp.suffix!="") mathOutput.mo(etp.suffix,MathMLTags.INFIX,MathMLTags.THICKMATHSPACE,MathMLTags.THICKMATHSPACE)
                    mathOutput.close
                    mathOutput.mathStyle = mc
                    output.endNoIndent(INSTREAM_FOREIGN_OBJECT)
                }
            }
        }
    }
	
}

class XslFoTableOfContentsPrinterTraveler(output:XslFoOutput) 
extends XslFoTextDocumentPrinterTraveler(output) {
    
    override def onEnter(node:Node[DocumentComponent]):Unit = {
        node.element match {
            case ns:NumSection => {
                val en:Enumerator = ns.enumerator
                val sc = counterFor(en)
                output.start(BLOCK)
                output.attr("font-size", Math.max(11-sc.currentLevel,7)+"pt")
                output.attr("margin-left",(2*sc.currentLevel)+"em")
                output.body
                output.append(sc.current.mkString("",".","."+SPACE))
                if(ns.title.isDefined){
                    output.append(ns.title.get)
                }
                ns.childrenOfType[Text](classOf[Text]).foreach(t => t match {
                    case ttt:TextToTranslate => {
                        output.append(Translator.translate(ttt.text,locale,ttt.dictionary).getOrElse(ttt.text))
                        output.append(SPACE)
                    }
                    case t:Text => {
                        output.append(t.textContent)
                        output.append(SPACE)
                    }
                    case _ =>
                })
                output.end(BLOCK)
                sc.in // counter level increment
            }
            case _ => Unit
        }
    }
        
    override def onExit(node:Node[DocumentComponent]):Unit = {
        node.element match {
            case ns:NumSection => {
                val sc = counterFor(ns.enumerator)
                sc.out //counter level decrement
                sc.next // counter increment
            }
            case _ =>
        }
    }
    
}




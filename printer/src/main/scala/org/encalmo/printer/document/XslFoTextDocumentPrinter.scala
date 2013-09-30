package org.encalmo.printer.document

import scala.collection._
import org.encalmo.common._
import org.encalmo.document._
import org.encalmo.expression._
import org.encalmo.printer.XslFoTags._
import org.encalmo.printer.expression._
import org.encalmo.printer._
import org.encalmo.style._
import org.encalmo.calculation.Results
import scala.Some
import org.encalmo.common.Node
import org.encalmo.document.TableOfContents
import org.encalmo.style.Style

/**
 * Prints document as xsl-fo text 
 * @author artur.opala
 */
object XslFoTextDocumentPrinter extends DocumentPrinter[XslFoOutput,String] {
	
	override def print(input:Document)(output:XslFoOutput = new XslFoOutput)(results: Results):XslFoOutput = {
		val t = new XslFoTextDocumentPrinterVisitor(output, results)
		input.visit(visitor = t)
		output
	}

}

/** 
 * Travels and prints document as xsl-fo text
 * @author artur.opala
 */
class XslFoTextDocumentPrinterVisitor(val output:XslFoOutput, results: Results, val counter: Option[MultiCounter] = None)
extends TreeVisitor[DocumentComponent]  with DocumentPrinterVisitor{

	val blockExprPrintStrategy:ExpressionPrintStrategy = output.preferences.expressionPrintStrategy match {
		case "table" => new ExpressionPrintAsTableStrategy(this)
		case _ => new ExpressionPrintAsTableStrategy(this)
	}
	
	var isInFlow:Boolean = false
	
	def tryStartPageSequence(chapter:Chapter, style:Style):Unit = {
		if(!isInFlow || chapter!=null){
			if(isInFlow || chapter!=null){
				endPageSequence()
			}
			output.start(PAGE_SEQUENCE)
			output.attr("master-reference", output.layout.id)
			output.appendBlockStyleAttributes(style, styleStack.top)
			output.body()
			if(chapter!=null){
				if(chapter.header!=null){
					output.start(STATIC_CONTENT)
					output.attr("flow-name","xsl-region-before")
					output.body()
					output.start(BLOCK)
                    output.attr("text-align","center")
                    output.attr("font-size","80%")
                    output.attr("line-height","125%")
                    output.attr("padding-bottom","0.5em")
                    output.attr("border-bottom","0.3pt solid black")
                    output.body()
					isInFlow = true
					chapter.header.visit(visitor=this)
					isInFlow = false
					output.end(BLOCK)
					output.end(STATIC_CONTENT)
				}
				if(chapter.footer!=null){
					output.start(STATIC_CONTENT)
					output.attr("flow-name","xsl-region-after")
					output.body()
                    output.start(BLOCK)
                    output.attr("text-align","right")
                    output.attr("border-top","1pt solid black")
                    output.body()
					isInFlow = true
					chapter.footer.visit(visitor=this)
					isInFlow = false
					output.startb(INLINE)
					output.tag(PAGE_NUMBER)
					output.end(INLINE)
					output.end(BLOCK)
					output.end(STATIC_CONTENT)
				}
			}
			output.start(FLOW)
			output.attr("flow-name","xsl-region-body")
			output.body()
			output.startb(BLOCK)
			isInFlow = true
		}
	}
	
	def endPageSequence() = {
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
                mathOutput.numberStyle = d.stylesConfig.numbers
				return
			}
			case chapter:Chapter => {
				tryStartPageSequence(chapter,chapter.style)
			}
            case toc:TableOfContents => {
                output.start(BLOCK)
                output.attr("margin-top","1em")
                output.attr("margin-bottom","1em")
                output.body()
                if(toc.parent.isDefined){
                    output.start(BLOCK)
                    output.attr("margin-bottom","0.3em")
                    output.attr("font-size",""+toc.style.font.size+"pt")
                    output.attr("font-weight","normal")
                    output.body()
                    output.append(toc.title)
                    output.end(BLOCK)
                    val counter = toc.parentOfType(classOf[NumSection]).map(ns => counterFor(ns.enumerator).copy())
                    toc.parent.get.visitChildren(visitor = new XslFoTableOfContentsPrinterVisitor(output, results, toc.levels, counter))
                }
                output.end(BLOCK)
            }
            case PageBreak => {
                output.start(BLOCK)
                output.attr("break-after","page")
                output.end()
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
						output.body()
						val ens = en.style
						if(ens!=null){
							output.start(INLINE)
							output.appendInlineStyleAttributes(en.style, styleStack.top)
							output.body()
						}
						
						output.append(sc.current.mkString("",".","."+output.SPACE))
						sc.in() // counter level increment
						if(ens!=null){
							output.end(INLINE)
						}
						if(ns.title.isDefined){
							output.start(INLINE)
							output.appendInlineStyleAttributes(ns.style, styleStack.top)
							output.body()
							output.append(ns.title.get)
                            output.end(INLINE)
						}
                        closestNumSection = Some(ns)
					}
					case s:Section => {
						output.start(BLOCK)
						output.appendBlockStyleAttributes(s.style, styleStack.top)
						output.body()
					}
					case ch:Character => {
						output.append(ch)
					}
					case t:TextContent => {
						if(t.customStyle!=null){
							output.start(INLINE)
							output.appendInlineStyleAttributes(t.customStyle, styleStack.top)
							output.body()
						}
						output.append(t.translate(locale))
                        if(t.customStyle!=null){
							output.end(INLINE)
						}
					}
					case expr:InlineExpr => {
						val ess:Seq[FormulaToPrint] = ExpressionToPrint.prepare(expr,results)
						ess.foreach(es => {
							if(expr.customStyle!=null){
								output.start(INLINE)
								output.appendInlineStyleAttributes(expr.customStyle, styleStack.top)
								output.attr("padding-end","1em")
								output.body()
							}
							es.foreach(etp => {
			                    printExpression(etp, expr.style)
			                })
							if(expr.customStyle!=null){
								output.end(INLINE)
							}
						})
					}
                    case req:Check => {
                        val ess:Seq[FormulaToPrint] = for(expression <- req.expressions) yield {
                            ExpressionToPrint.prepare(expression, req, results, printStyleOfRequire(expression,results))
                        }
                        if(!ess.isEmpty){
                            blockExprPrintStrategy.print(node,ess,req.isPrintDescription)
                        }
                    }
					case expr:BlockExpr => {
						val ess:Seq[FormulaToPrint] = ExpressionToPrint.prepare(expr,results)
						if(!ess.isEmpty){
							blockExprPrintStrategy.print(node,ess,expr.isPrintDescription)
						}
					}
					case a:Assertion => {
						val result = a.evaluate(results.cache)
						val s = Section(DefaultStyle.marginTop(5).fontSmaller,result._2:_*)
						s.visit(visitor = this)
                    }
                    case symb:Symb => {
                        if(symb.customStyle!=null){
                            output.start(INLINE)
                            output.appendInlineStyleAttributes(symb.customStyle, styleStack.top)
                            output.attr("padding-end","1em")
                            output.body()
                        }
                        printExpression(ExpressionToPrint(symb.expression,symb.customStyle,"","",symb.stylesConfig), symb.style)
                        if(symb.customStyle!=null){
                            output.end(INLINE)
                        }
                    }
                    case image:Image => {
                        output.startb(BLOCK)
                        output.start(EXTERNAL_GRAPHIC)
                        output.attr("src",image.source)
                        output.attr("text-align","center")
                        output.end()
                        output.end(BLOCK)
                    } case chl: Checklist => {
                        val (succeses,failures) = chl.findAndPartitionRequirementsFormulas(results)
                        val errorsToPrint:Seq[FormulaToPrint] = for(f <- failures.take(chl.limit)) yield {
                            val expressions = ExpressionToPrint.prepare(f,ExpressionToPrint.NOT_RIGHT_NOR_LEFT,chl.customStyle,chl)
                            FormulaToPrint(f.expression, expressions, FormulaPrintStyle.ERROR)
                        }
                        if(!errorsToPrint.isEmpty){
                            output.startb(BLOCK)
                            this.onEnter(Node(node,Text(chl.style,"requirements_not_fulfilled",Translator.defaultDictionary),0))
                            output.end(BLOCK)
                            blockExprPrintStrategy.print(node,errorsToPrint,true)
                        } else {
                            val limitStatesToPrint:Seq[FormulaToPrint] = for(f <- succeses.take(chl.limit)) yield {
                                val expressions = ExpressionToPrint.prepare(f,ExpressionToPrint.NOT_RIGHT_NOR_LEFT,chl.customStyle,chl)
                                FormulaToPrint(f.expression, expressions, FormulaPrintStyle.NORMAL)
                            }
                            if(!limitStatesToPrint.isEmpty){
                                output.startb(BLOCK)
                                this.onEnter(Node(node,Text(chl.style,"top_decisive_limit_states",Translator.defaultDictionary),1))
                                this.onEnter(Node(node,Text(" ("+Math.min(limitStatesToPrint.size,chl.limit)+")")))
                                output.end(BLOCK)
                                blockExprPrintStrategy.print(node,limitStatesToPrint,true)
                            }
                        }
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
				output.append(output.SPACE)
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
		styleStack.pop()
		node.element match {
			case d:Document => {
				endPageSequence()
			}
			case c:Chapter => {
				endPageSequence()
			}
			case ns:NumSection => {
				output.end(BLOCK)
				val sc = counterFor(ns.enumerator)
				sc.out() //counter level decrement
				sc.next() // counter increment
			}
			case s:Section => {
				output.end(BLOCK)
			}
			case _ =>
		}
	}
    
    /** Print expression as table */
    class ExpressionPrintAsTableStrategy (
		traveler:XslFoTextDocumentPrinterVisitor
	) extends ExpressionPrintStrategy {
    	
    	override def print(node:Node[DocumentComponent], ess:Seq[FormulaToPrint], isPrintDescription: Boolean) = {
    		val parentNumSection = node.element.parentOfType[NumSection](classOf[NumSection])
            val stylesConfig = node.element.stylesConfig
            val counter: Option[Counter] = closestNumSection.map(_.expressionCounter)
            val rowStyle:Style = stylesConfig.block
    		output.start(TABLE)
    		output.attr("table-layout","fixed")
    		output.attr("width","100%")
    		if(node.element.isFirstBlockComponent){
    			output.attr("keep-with-previous","always")
			    parentNumSection.map(x => output.attr("space-before",x.style.paragraph.spaceBefore*0.8))
			}
    		output.body()
    		output.tableColumn("2.5","em")
    		output.tableColumn("proportional-column-width(40)","")
    		output.tableColumn("proportional-column-width(60)","")
    		output.start(TABLE_BODY)
			output.body()
			for(es <- ess){
				output.startb(TABLE_ROW)
				val bullet = counter.map(_.item+")").getOrElse(null)
				printFormula(es, node.element.style, isPrintDescription, bullet, rowStyle, false, stylesConfig)
                counter.foreach(_.increment())
				output.end(TABLE_ROW)
			}
            output.end(TABLE_BODY)
            output.end(TABLE)
    	}
    	
    	def printFormula(ftp:FormulaToPrint, style:Style, printDescription:Boolean, bullet:String, tableRowStyle: Style, secondTableRow:Boolean, stylesConfig:StylesConfig){
		    if(!ftp.isEmpty){
	        	val description:Option[String] = ftp.expression match {
	        		case s:SymbolLike => s.symbol.localizedDescription(locale)
	        		case _ => None
	        	}
                val rowStyle: Style = ftp.printStyle match {
                    case FormulaPrintStyle.BOLD =>  stylesConfig.requirement_true
                    case FormulaPrintStyle.ERROR =>  stylesConfig.requirement_false
                    case _ => tableRowStyle
                }
	        	val printable:Boolean = ftp.expression.printable
	        	val isPrintDescription = printDescription && description.isDefined && description.get!=""
	        	val paddingTop = rowStyle.paragraph.spaceBefore
	        	val paddingBottom = rowStyle.paragraph.spaceAfter
	        	val indent:Double = if(style!=null && style.paragraph.width>0) style.paragraph.width else 30
		    	val isCell1 = true
		        val isCell2 = isPrintDescription
                val descStyle = stylesConfig.symbolDescription
	        	val leafs:Int = ftp.map(x => x.expression.countTreeLeafs).sum
	        	val twoRows:Boolean = !secondTableRow && (leafs>15 || (description match {
	        		case Some(d) => d.size>150
	        		case None => false
	        	}))
	        	val twoTableRows = !secondTableRow && (isCell2 && twoRows && leafs<15)
		    	if(isCell1){
					output.start(TABLE_CELL)
                    output.attr("border-bottom",toCSS(rowStyle.paragraph.border.bottom))
                    output.attr("border-top",toCSS(rowStyle.paragraph.border.top))
                    output.attr("border-left",toCSS(rowStyle.paragraph.border.left))
					output.body()
			        output.start(BLOCK)
			        output.attr("padding-top",paddingTop,"pt")
			        output.attr("padding-bottom",paddingBottom,"pt")
			        output.attr("margin-right","0.7em")
			        output.attr("text-align","right")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.body()
			        if(bullet!=null) output.append(bullet)
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
		    	}
		        if(isCell2 && !twoRows){
			        output.start(TABLE_CELL)
			        if(!twoTableRows) {
			            output.attr("border-bottom",toCSS(rowStyle.paragraph.border.bottom))
			            output.attr("border-top",toCSS(rowStyle.paragraph.border.top))
			        }
			        output.body()
			        output.start(BLOCK)
			        output.attr("margin-right","3pt")
			        output.attr("padding-top",paddingTop,"pt")
			        output.attr("padding-bottom",paddingBottom,"pt")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.appendHyphenationStyleAttributes(descStyle,styleStack.top)
		        	output.attr("keep-together.within-page","always")
			        output.body()
			        if(!secondTableRow) {
			        	output.append(description)
			        }
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
		    	}
		        output.start(TABLE_CELL)
		        if(!secondTableRow && (!isCell2 || twoRows)){
		        	val ncs:Int = 2 + {if(isCell1) 0 else 1}
		        	output.attr("number-columns-spanned",ncs)
                }
				if(!twoTableRows){
				    output.attr("border-bottom",toCSS(rowStyle.paragraph.border.bottom))
                    output.attr("border-top",toCSS(rowStyle.paragraph.border.top))
                    output.attr("border-right",toCSS(rowStyle.paragraph.border.right))
				}
			    output.attr("vertical-align","middle")
		        output.body()
		        if(isCell2 && twoRows){
		        	output.start(BLOCK)
		        	output.attr("keep-together.within-page","always")
			        output.attr("margin-top",paddingTop,"pt")
			        output.appendInlineStyleAttributes(descStyle,styleStack.top)
			        output.body()
			        output.append(description)
			        output.end(BLOCK)
		        }
				if(twoTableRows){
					output.end(TABLE_CELL)
					output.end(TABLE_ROW)
					output.start(TABLE_ROW)
					output.attr("keep-with-previous","always")
					output.body()
					printFormula(ftp, style, printDescription, "", rowStyle, true, stylesConfig)
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
			        output.body()
			        ftp.foreach(etp => {
	    				printExpression(etp, style)
	    			})
			        output.end(BLOCK)
			        output.end(TABLE_CELL)
				}
		    }
		}
    	
	}
        
    def printExpression(etp:ExpressionToPrint, style:Style, parentNode:Node[Expression] = null, position:Int=0, span:Boolean = true):Unit = {
        if(etp.expression.printable){
            val numberOfLeafs:Int = etp.expression.countTreeLeafs
            etp.expression match {
                case tr:Transparent => {
                    val node = Node[Expression](parentNode,tr,position)
                    tr.children.size match {
                        case 0 => Unit
                        case 1 => printExpression(ExpressionToPrint(tr.children.head,etp.style,etp.prefix,etp.suffix,etp.stylesConfig),style,node,0,span)
                        case _ => {
                            printExpression(ExpressionToPrint(tr.children.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
                            tr.children.tail.foreach(e => {
                                printExpression(ExpressionToPrint(e,etp.style,null,null,etp.stylesConfig),style,node,1,false)
                            })
                            if(etp.suffix!=null)output.append(etp.suffix)
                        }
                    }
                }
                case mio:MultipleInfixOperation if mio.args.size > 1 && numberOfLeafs>=10 => {
                    val node = Node[Expression](parentNode,mio,position)
                    printExpression(ExpressionToPrint(mio.args.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
                    mio.args.tail.foreach(e => {
                        printExpression(ExpressionToPrint(e,etp.style,output.convertOperator(mio.operator),null,etp.stylesConfig),style,node,1,false)
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
                    output.attr("padding","1.5pt 0")
                    output.body()
                    mathOutput.open()
                    if(etp.prefix!=null && etp.prefix!="") {
                        mathOutput.mo(etp.prefix,MathMLTags.INFIX,MathMLTags.THICKMATHSPACE,MathMLTags.THICKMATHSPACE)
                        if(etp.prefix=="⇒")mathOutput.thickspace()
                    }
                    etp.expression.visit(visitor = ept, parentNode = parentNode, position = position)
                    if(etp.suffix!=null && etp.suffix!="") {
                        if(etp.suffix=="⇒")mathOutput.thickspace()
                        mathOutput.mo(etp.suffix,MathMLTags.INFIX,MathMLTags.THICKMATHSPACE,MathMLTags.THICKMATHSPACE)
                    }
                    mathOutput.close()
                    mathOutput.mathStyle = mc
                    output.endNoIndent(INSTREAM_FOREIGN_OBJECT)
                }
            }
        }
    }

    def toCSS(b:Border):String = s"${b.width}pt ${b.style} ${Style.toHex(b.color)}"
	
}

class XslFoTableOfContentsPrinterVisitor(output:XslFoOutput, results: Results, levels: Int = 3, counter: Option[MultiCounter] = None)
extends XslFoTextDocumentPrinterVisitor(output, results, counter) {

    val maxLevel = levels + counter.map(_.currentLevel).getOrElse(0)
    val expectedEnumerator = counter.map(_.enumerator).getOrElse(null)
    
    override def onEnter(node:Node[DocumentComponent]):Unit = {
        node.element match {
            case ns:NumSection if ns.enumerator eq expectedEnumerator => {
                val en:Enumerator = ns.enumerator
                val sc = counterFor(en)
                if(sc.currentLevel < maxLevel){
                    output.start(BLOCK)
                    output.attr("font-size", Math.max(11-sc.currentLevel,7)+"pt")
                    output.attr("font-weight","normal")
                    output.attr("margin-left",(2*sc.currentLevel)+"em")
                    output.body()
                    output.append(sc.current.mkString("",".","."+output.SPACE))
                    if(ns.title.isDefined){
                        output.append(ns.title.get)
                    }
                    ns.childrenOfType[Text](classOf[Text]).foreach(t => t match {
                        case t:TextContent => {
                            output.append(t.translate(locale))
                            output.append(output.SPACE)
                        }
                        case _ =>
                    })
                    output.end(BLOCK)
                }
                sc.in() // counter level increment
            }
            case _ => Unit
        }
    }
        
    override def onExit(node:Node[DocumentComponent]):Unit = {
        node.element match {
            case ns:NumSection => {
                val sc = counterFor(ns.enumerator)
                sc.out() //counter level decrement
                sc.next() // counter increment
            }
            case _ =>
        }
    }
    
}




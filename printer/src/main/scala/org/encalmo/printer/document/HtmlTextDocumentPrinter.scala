package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression._
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._
import org.encalmo.printer.HtmlTags._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Stack
import scala.collection._
import org.encalmo.style.{StylesConfigSymbols, Style, DefaultStyle, StylesConfig}
import org.encalmo.calculation.Results
import org.w3c.dom.mathml.MathMLPredefinedSymbol
import scala.Some
import org.encalmo.common.Node
import org.encalmo.document.TableOfContents
import org.encalmo.style.Style
import org.encalmo.style.StylesConfig
import scala.Some
import org.encalmo.common.Node
import org.encalmo.document.TableOfContents
import org.encalmo.style.Style
import org.encalmo.style.StylesConfig
import java.text.DecimalFormat

/**
 * Prints document as html5 text 
 * @author artur.opala
 */
object HtmlTextDocumentPrinter extends DocumentPrinter[HtmlOutput,String] {
	
	override def print(input:Document)(output:HtmlOutput = new HtmlOutput)(results:Results):HtmlOutput = {
		val t = new HtmlTextDocumentPrinterVisitor(output, results)
		input.visit(visitor = t)
		output
	}

}

/** 
 * Visits and prints document as html document
 * @author artur.opala
 */
class HtmlTextDocumentPrinterVisitor(val output:HtmlOutput, results: Results, val counter: Option[MultiCounter] = None)
extends TreeVisitor[DocumentComponent] with DocumentPrinterVisitor {
	
	val embededStyles = """ 
body {font-family:sans-serif;font-size:12pt;background-color: #FFFAD8;}
table {border-collapse:collapse;margin:5pt;background-color: #FFFDEF;border: 1px solid #FFBE32;} 
div {padding:5pt 0 2pt 0}
.et {width:100%}
.et td {border-top: 1px solid #FFBE32;border-bottom: 1px solid #FFBE32;border-right: 1px dotted #FFBE32;border-left: 1px dotted #FFBE32;padding:2pt 5pt;}
.ec1 {width:30%; margin-left:2em; text-align:left; vertical-align:top}
.ec2 {width:5%; font-size:12pt; font-weight: bold; text-align:center;}
.ec3 {width:65%; font-size:10pt}
	    """
	    
	val blockExprPrintStrategy:ExpressionPrintStrategy = output.preferences.formulaLayoutStrategy match {
		case "table" => new ExpressionPrintAsTableStrategy(this)
		case _ => new ExpressionPrintAsTableStrategy(this)
	}

    override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case nvc:NonVisualComponent => return
			case d:Document => {
				output.startb(STYLE)
				val allStyles = d.allStyles
				if(output.preferences.hasNotCustomStyleSheet && output.preferences.isRenderHeaderAndBodyTags){
				    output.append(embededStyles)
				}
				if (output.preferences.ignoreDocumentStyles) {
          if (output.preferences.hasCustomStyleSheet) {
            output.append(output.preferences.customStyleSheet)
          }
        } else {
          d.stylesConfig.all.foreach(allStyles.add)
          allStyles.foreach(output.styledef(_, d.stylesConfig))
        }
				output.end(STYLE)
				return
			}
			case chapter:Chapter => {
			    output.tag(DIV,"chapter")
			}
			case toc:TableOfContents => {
                if(toc.parent.isDefined){
                    val counter = toc.parentOfType(classOf[NumSection]).map(ns => counterFor(ns.enumerator).copy())
                    output.startb(DIV,"toc")
                    toc.parent.get.visitChildren(visitor = new HtmlTableOfContentsPrinterVisitor(output, results, toc.levels, counter))
                    output.end(DIV)
                }
            }
            case ns:NumSection => {
                val en:Enumerator = ns.enumerator
                val sc = counterFor(en)
                val label = sc.current.mkString("",".",".")
                output.startb(DIV,ns.styleClassId)
                output.start(ANCHOR)
                output.attr("name",label)
                output.body()
                output.append(" ")
                output.end(ANCHOR)
                output.startb(SPAN,"caption")
                val ens = en.style
                if(ens!=null){
                    output.startb(SPAN,en.styleClassId)
                }
                output.append(label,output.SPACE)
                sc.in() // counter level increment
                if(ens!=null){
                    output.end(SPAN)
                }
                if(ns.title.isDefined){
                    output.append(ns.title.get)
                }
                output.end(SPAN)
                closestNumSection = Some(ns)
            }
            case s:Section => {
                output.startb(DIV, s.styleClassId)
            }
            case ch:Character => {
                output.append(ch)
            }
            case t:TextContent => {
                if(t.customStyle!=null){
                    output.startb(SPAN,t.customStyleClassId)
                }
                output.append(t.translate(locale))
                if(t.customStyle!=null){
                    output.end(SPAN)
                }
            }
            case expr:InlineExpr => {
                val ess:Seq[FormulaToPrint] = ExpressionToPrint.prepare(expr,results)
                ess.foreach(es => {
                    if(expr.customStyle!=null){
                        output.start(SPAN,expr.customStyleClassId)
                        output.attr("style","padding-end:1em")
                        output.body()
                    }
                    es.foreach(etp => {
                        printExpression(etp, expr.style)
                    })
                    if(expr.customStyle!=null){
                        output.end(SPAN)
                    }
                })
            }
            case req:Check => {
                val ess:Seq[FormulaToPrint] = (for(expression <- req.expressions) yield {
                    ExpressionToPrint.prepare(expression, req, results, printStyleOfCheck(expression,results))
                }).filter(_.isDefined).map(_.get)
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
                val s = Section(a.style,result._2:_*)
                s.parent = a.parent
                s.visit(visitor = this)
            }
            case symb:Symb => {
                if(symb.customStyle!=null){
                    output.start(SPAN,symb.customStyleClassId)
                    output.attr("style","padding-end:1em")
                    output.body()
                }
                printExpression(ExpressionToPrint(symb.expression,symb.customStyle,"","",symb.stylesConfig), symb.style)
                if(symb.customStyle!=null){
                    output.end(SPAN)
                }
            }
            case image:Image => {
                output.start(DIV,image.customStyleClassId)
                output.body()
                output.start(IMG)
                output.attr("src",image.source)
                output.end()
                output.end(DIV)
            }
            case chl: Checklist => {
                val (limitStates,failures) = chl.findAndPartitionRequirementsFormulas(results)
                val errorsToPrint:Seq[FormulaToPrint] = for(f <- failures.take(chl.limit)) yield {
                    val expressions = ExpressionToPrint.prepare(f,ExpressionToPrint.NOT_RIGHT_NOR_LEFT,chl.customStyle,chl)
                    val ratio: String = ratioOfAssertFormula(expressions)
                    val ftp = FormulaToPrint(f.expression, expressions, FormulaPrintStyle.ERROR, Option(ratio))
                    Console.err.println("Failure: "+f.face)
                    ftp
                }
                if(!errorsToPrint.isEmpty){
                    output.startb(DIV,"errorslist")
                    this.onEnter(Node(node,Text(chl.style,"requirements_not_fulfilled",Translator.defaultDictionary),0))
	                this.onEnter(Node(node,Text(" ("+errorsToPrint.size+" / "+failures.size+"):")))
                    blockExprPrintStrategy.print(node,errorsToPrint,true)
                } else {
                    output.startb(DIV,"checklist")
                    val limitStatesToPrint:Seq[FormulaToPrint] = for(f <- limitStates.take(chl.limit)) yield {
                        val expressions = ExpressionToPrint.prepare(f,ExpressionToPrint.NOT_RIGHT_NOR_LEFT,chl.customStyle,chl)
                        val ratio: String = ratioOfAssertFormula(expressions)
                        FormulaToPrint(f.expression, expressions, FormulaPrintStyle.NORMAL, Option(ratio))
                    }
                    if(!limitStatesToPrint.isEmpty){
                        this.onEnter(Node(node,Text("top_decisive_limit_states",Translator.defaultDictionary),1))
                        this.onEnter(Node(node,Text(" ("+limitStatesToPrint.size+" / "+limitStates.size+"):")))
                        blockExprPrintStrategy.print(node,limitStatesToPrint,true)
                    }
                }
                output.end(DIV)
            }
            case _ => {}
		}
		// push current style on the stack
		styleStack.push(node.element.style)
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
				
			}
			case c:Chapter => {
				output.end(P)
			}
			case ns:NumSection => {
				output.end(DIV)
				val sc = counterFor(ns.enumerator)
				sc.out() //counter level decrement
				sc.next() // counter increment
			}
			case s:Section => {
				output.end(DIV)
			}
			case _ =>
		}
	}
    
    /** Print expression as table */
    class ExpressionPrintAsTableStrategy (
		traveler:HtmlTextDocumentPrinterVisitor
	) extends ExpressionPrintStrategy {
    	
    	override def print(node:Node[DocumentComponent], ess:Seq[FormulaToPrint], isPrintDescription: Boolean) = {
    		val parentNumSection = node.element.parentOfType[NumSection](classOf[NumSection])
    		val stylesConfig = node.element.stylesConfig
            val counter: Option[Counter] = closestNumSection.map(_.expressionCounter)

    		output.start(TABLE,"et")
    		if(node.element.isFirstBlockComponent){
			    parentNumSection.map(x => output.attr("style","space-before:",x.style.paragraph.spaceBefore*0.8))
			}
    		output.attr("cellpadding","2")
    		output.attr("cellspacing","0")
    		output.body()
			for(es <- ess){
				output.startb(TR)
				val bullet = counter.map(_.item+")").getOrElse(null)
				printFormula(es, node.element.style, isPrintDescription, bullet, stylesConfig)
                counter.foreach(_.increment())
				output.end(TR)
			}
            output.end(TABLE)
    	}
    	
    	def printFormula(ftp:FormulaToPrint, style:Style, printDescription:Boolean, bullet:String, stylesConfig:StylesConfig){
            if(!ftp.isEmpty){
	        	val description:Option[String] = concatenate(
                    ftp.prefix,
                    ftp.expression match {
                        case s:SymbolLike => s.symbol.localizedDescription(locale)
                        case _ => None
                    },
                    ftp.suffix
                )
                val classIdSuffix: String = ftp.printStyle match {
                    case FormulaPrintStyle.BOLD =>  "bd"
                    case FormulaPrintStyle.ERROR =>  "er"
                    case _ => ""
                }
	        	val isPrintDescription = printDescription && description.isDefined
		    	val isCell1 = bullet!=null
		        val isCell2 = isPrintDescription
                val descStyle = stylesConfig.symbolDescription
		    	if(isCell1 || isCell2){
					output.startb(TD,"ec1"+{if(isPrintDescription)"w" else "n"}+classIdSuffix)
					output.startb(SPAN,stylesConfig.matchStyleClassId(descStyle))
			        output.append(bullet)
			        if(isPrintDescription) {
                        output.append("&nbsp;")
                        output.append(description)
                    }
			        output.end(SPAN)
			        output.end(TD)
		    	}
		        ftp.head.expression match {
	        		case s:SymbolLike => {
	        		    printTwoColumns(classIdSuffix, isCell2, isCell1)
	        		}
                    case a:Assert => {
                        printTwoColumns(classIdSuffix, isCell2, isCell1)
                    }
	        		case _ => {
	        		    output.start(TD,"ec3"+classIdSuffix)
                        val ncs:Int = 2 + (if(!isCell2 && !isCell1) 1 else 0)
                        output.attr("colspan",ncs)
				        output.body()
				        ftp.foreach(etp => {
		    				printExpression(etp, style)
		    			})
				        output.end(TD)
	        		}
	        	}
		        
		    }

            def printTwoColumns(classIdSuffix: String, isCell2: Boolean, isCell1: Boolean) {
                output.startb(TD, "ec2" + classIdSuffix)
                printExpression(ftp.head, style)
                output.end(TD)
                output.start(TD, "ec3" + classIdSuffix)
                if (!isCell2) {
                    val ncs: Int = 2 + {
                        if (isCell1) 0 else 1
                    }
                    output.attr("colspan", ncs)
                }
                output.body()
                ftp.tail.foreach(etp => {
                    printExpression(etp, style)
                })
                output.end(TD)
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
			                if(span) output.startb(SPAN, etp.styleClassId)
			                printExpression(ExpressionToPrint(tr.children.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
			                tr.children.tail.foreach(e => {
                                printExpression(ExpressionToPrint(e,etp.style,null,null,etp.stylesConfig),style,node,1,false)
                            })
                            if(etp.suffix!=null)output.append(etp.suffix)
                            if(span) output.end(SPAN)
			            }
			        }
			    }
			    case mio:MultipleInfixOperation if mio.args.size > 1 && numberOfLeafs>=10 => {
			        if(span) output.startb(SPAN, etp.styleClassId)
			        val node = Node[Expression](parentNode,mio,position)
			        printExpression(ExpressionToPrint(mio.args.head,etp.style,etp.prefix,null,etp.stylesConfig),style,node,0,false)
			        mio.args.tail.foreach(e => {
			            printExpression(ExpressionToPrint(e,etp.style,output.convertOperator(mio.operator),null,etp.stylesConfig),style,node,1,false)
			        })
			        if(etp.suffix!=null)output.append(etp.suffix)
			        if(span) output.end(SPAN)
			    }
			    case _ => {
			        if(span) output.startb(SPAN, etp.styleClassId)
                    mathOutput.open()
                    if(etp.prefix!=null && etp.prefix!="") {
                        mathOutput.mo(etp.prefix,MathMLTags.INFIX,null,MathMLTags.THICKMATHSPACE)
                        if(etp.prefix=="⇒")mathOutput.append(MathMLTags.ENTITY_THIN_SPACE)
                    }
                    etp.expression.visit(visitor = ept, parentNode = parentNode, position = position)
                    if(etp.suffix!=null && etp.suffix!="") {
                        if(etp.suffix=="⇒")mathOutput.append(MathMLTags.ENTITY_THIN_SPACE)
                        mathOutput.mo(etp.suffix,MathMLTags.INFIX,MathMLTags.THICKMATHSPACE,null)
                    }
                    mathOutput.close()
                    if(span) output.end(SPAN)
			    }
			}
		}
	}
	
}

class HtmlTableOfContentsPrinterVisitor(val output:HtmlOutput, results: Results, levels: Int = 3, val counter: Option[MultiCounter] = None)
extends TreeVisitor[DocumentComponent] with DocumentPrinterVisitor {

    val maxLevel = levels + counter.map(_.currentLevel).getOrElse(0)
    val expectedEnumerator = counter.map(_.enumerator).getOrElse(null)
    
    override def onEnter(node:Node[DocumentComponent]):Unit = {
        node.element match {
            case ns:NumSection if ns.enumerator eq expectedEnumerator => {
                val en:Enumerator = ns.enumerator
                val sc = counterFor(en)
                if(sc.currentLevel < maxLevel){
                    val label = sc.current.mkString("",".",".")
                    output.startb(SPAN,"toc"+sc.currentLevel)
                    output.start(ANCHOR)
                    output.attr("href","#",label)
                    output.body()
                    output.append(label,output.SPACE)
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
                    output.end(ANCHOR)
                    output.end(SPAN)
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




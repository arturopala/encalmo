package org.encalmo.printer.document

import org.encalmo.common._
import org.encalmo.expression.Symbol
import org.encalmo.expression.SymbolLike
import org.encalmo.printer._
import org.encalmo.printer.expression._
import org.encalmo.document._
import scala.collection.mutable.LinkedHashMap
import org.encalmo.calculation.Results
import scala.collection.mutable

/**
 * Prints document as plain text 
 * @author artur.opala
 */
object PlainTextDocumentPrinter extends TextDocumentPrinter {
	
	override def print(input:Document)(output:TextOutput = new TextOutput)(results: Results):TextOutput = {
		val t = new PlainTextDocumentPrinterTraveler(output, results)
		try{
			input.visit(visitor = t)
		}
		catch {
			case e: Throwable => {
				output.append("\r\n")
				output.append(e.getClass.getName+": "+e.getMessage)
			}
		}
		output
	}

}

/**
 * Travels and prints document as plain text 
 * @author artur.opala
 */
class PlainTextDocumentPrinterTraveler(output:TextOutput, results: Results)
extends TreeVisitor[DocumentComponent] {
	
	val w = output.asWriter
	val locale = output.locale
	val ept = new PlainTextExpressionPrinterTraveler(output)
	val dfs = java.text.DecimalFormatSymbols.getInstance(locale)
	/** Section counters map */
	private val counterMap:mutable.LinkedHashMap[Enumerator,SectionCounter] = mutable.LinkedHashMap[Enumerator,SectionCounter]()
	
	/** Returns counter linked to the enumerator */
	private def counterFor(en:Enumerator):SectionCounter = {
		var sco = counterMap.get(en)
		if(!sco.isDefined){
			sco = Some(SectionCounter(en))
			counterMap.put(en,sco.get)
		}
		sco.get
	}
	
	val SPACE = " "
	val COMMA = dfs.getPatternSeparator
	val SEP = "\r\n"
	val TAB = "  "
		
	def write(ch:Char) = {
		w.write(ch)
        canNewLine = true
	}
	
	def write(os:Option[String]) = {
		os.map(s => {
			w.write(s)
        })
		canNewLine = true
	}
		
	def write(sq:String*) = {
		for(s <- sq){
			w.write(s)
        }
		canNewLine = true
	}
	
	def writeTabs() = {
		for(x <- 1 to tabs){
			w.write(TAB)
		}
	}
	
	def writeLineEnd() = {
		if(canNewLine){
			w.write(SEP)
            writeTabs()
			canNewLine = false
		}
	}
	
	def writeExpression(se:FormulaToPrint): Unit = {
		se.foreach(writeExpressionPart)
	}
	
	def writeExpressionPart(etp:ExpressionToPrint): Unit = {
		if(etp.expression.printable){
		    if(etp.prefix!=null){
		        w.write(SPACE)
		        w.write(etp.prefix)
		        w.write(SPACE)
		    }
			etp.expression.visit(visitor = ept)
			if(etp.suffix!=null){
			    w.write(SPACE)
			    w.write(etp.suffix)
			    w.write(SPACE)
			}
		}
	}
	
	def plus() = {tabs = tabs+1}
	def minus() = {tabs = tabs-1}
	
	var tabs:Int = 0
	var canNewLine:Boolean = true
	
	override def onEnter(node:Node[DocumentComponent]):Unit = {
		node.element match {
			case ch:Character => {
				write(ch.text)
			}
			case tc:TextContent => {
				if(tc.text!=null){
					write(tc.translate(locale))
				}
			}
			case _ =>
		}
		node.element match {
			case d:Document => {
				//plus
				if(d.title!=null){
					//writeLineEnd
					//write(d.title)
					//writeLineEnd
				}
			}
			case c:Chapter => {
				//plus
				//writeLineEnd
				//write("")
				//writeLineEnd
			}
			case ns:NumSection => {
				val sc = counterFor(ns.enumerator)
				val max = 2 - ns.enumeratorLevel
				for(x <- 0 to (if(max>0) max else 1)){
					writeLineEnd()
					canNewLine = true
                }
				canNewLine = false
                write(sc.current.mkString("",".","."+SPACE))
				sc.in() // counter level increment
				if(ns.title.isDefined){
				    write(ns.title.get)
				}
				plus()
			}
			case s:Section => {
				writeLineEnd()
				plus()
			}
			case expr:InlineExpr => {
				val ess:Seq[FormulaToPrint] = ExpressionToPrint.prepare(expr,results)
				ess.foreach(e => {write(" ");writeExpression(e);write(" ")})
			}
			case expr:BlockExpr => {
				val ess:Seq[FormulaToPrint] = ExpressionToPrint.prepare(expr,results)
				//plus
			    ess.foreach(es => {
			        canNewLine = true
                    writeLineEnd()
                    es.head.expression match {
			        	case s:SymbolLike if s.symbol.hasLocalizedDescription(locale) => {
			        		write(s.symbol.localizedDescription(locale))
	                        plus()
	                        if(s.symbol.printable) {
	                        	canNewLine = true
                                writeLineEnd()
                            }
	                        writeExpression(es)
	                        minus()
			        	}
			        	case _ => {
			        		writeExpression(es)
			        	}
			        }
		        })
			    //minus
			    canNewLine = true
			    //writeLineEnd
			}
			case a:Assertion => {
				val result = a.evaluate(results.cache)
				val s = Section(a.style,result._2:_*)
				s.visit(visitor = this)
                result._1 match {
					case None => throw new IllegalStateException
					case Some(b) if !b => throw new IllegalStateException("")
					case _=>
				}
				canNewLine = true
            }
            case symb: Symb => {
                write(" ")
                writeExpressionPart(ExpressionToPrint(symb.expression,symb.customStyle,"",""))
                write(" ")
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
				//minus
			}
			case c:Chapter => {
				//minus
			}
			case ns:NumSection => {
				canNewLine = true
				minus()
				val sc = counterFor(ns.enumerator)
				sc.out() //counter level decrement
				sc.next() // counter increment
			}
			case s:Section => {
				canNewLine = true
				minus()
			}
			case _ =>
		}
	}
	
}


package org.encalmo.printer.expression

import org.encalmo.printer._
import org.encalmo.expression._
import org.encalmo.calculation.{ResultsCache, EvalAt}
import org.encalmo.common._
import org.encalmo.common.Translator

/**
 * Prints expressions as plain text 
 * @author artur.opala
 */
object PlainTextExpressionPrinter extends TextExpressionPrinter {
	
	override def print(e:Expression)(output:TextOutput = new TextOutput)(any: AnyRef = new Object()):TextOutput = {
		val t = new PlainTextExpressionPrinterTraveler(output)
		e.visit(visitor = t)
		output
	}

}

/**
 * Simple Traveler printing expression as plain text
 * @author artur.opala
 */
class PlainTextExpressionPrinterTraveler(output:TextOutput) extends TreeVisitor[Expression] {
	
	val w = output.asWriter
	val locale = output.locale
	
	lazy val integerFormat1:java.text.NumberFormat = new java.text.DecimalFormat("###,###,###,###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val dimensionFormat:java.text.NumberFormat = new java.text.DecimalFormat("0.#",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat1:java.text.NumberFormat = new java.text.DecimalFormat(".#",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat2:java.text.NumberFormat = new java.text.DecimalFormat(".##",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat3:java.text.NumberFormat = new java.text.DecimalFormat(".###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat4:java.text.NumberFormat = new java.text.DecimalFormat(".####",java.text.DecimalFormatSymbols.getInstance(locale))
	
	def writeOpeningBracket() = w.write('(')

    def writeClosingBracket() = w.write(')')

    def writeSpace() = w.write(' ')

    def writeSymbol(s:Symbol) = w.write(s.simpleFace)
	def writeNumber(n:Number) = {
		val nf:NumberFormatted = n.formattedForPrint
		if (nf.isNegative)w.write("-")
        w.write(integerFormat1.format(nf.integer))
		if(nf.fraction>0){
			nf.decimals match {
				case 1 => w.write(fractionFormat1.format(nf.fraction))
				case 2 => w.write(fractionFormat2.format(nf.fraction))
				case 3 => w.write(fractionFormat3.format(nf.fraction))
				case _ => w.write(fractionFormat4.format(nf.fraction))
			}
		}
		if(nf.hasExponent && nf.exponent!=0) {
			w.write("E")
			w.write(""+nf.exponent)
		}
	}
	def writeListSeparator() = w.write(';')

    def writeOpeningBracketIfNeeded(node:Node[Expression],o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeOpeningBracket()
		}
	}
	
	def writeClosingBracketIfNeeded(node:Node[Expression],o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeClosingBracket()
		}
	}
	
	def isBracketNeeded(node:Node[Expression],o:Operation):Boolean = {
		if(node.parent!=null){
			node.parent.element match {
				case o:abs => false
				case po:Operation => po.precedence>o.precedence && (node.position>0 || po.precedence-o.precedence>5)
				case _ => false
			}
		}else{
			false
		}
	}
	
	override def onEnter(node:Node[Expression]):Unit = {
	    if(node.parent!=null){
    	    node.parent.element match {
    	        case ct:CaseTest => {
                    w.write(ct.operator(node.position))
                }
    	        case _ =>
    	    }
	    }
	    node.element match {
    		case sl: SymbolLike => {
    		    if(node.parent!=null){
        		    node.parent.element match {
        		        case ev:EvalAt =>   w.write(sl.symbol.simpleFaceNoArgs)
        		        case _ =>  w.write(sl.symbol.simpleFace)
        		    }
    		    }else{
    		        w.write(sl.symbol.simpleFace)
    		    }
    		}
    		case n:Number => writeNumber(n)
    		case tv: TextValue => w.write(tv.text)
    		case o:Operation => {
    			writeOpeningBracketIfNeeded(node,o)
    			o match {
    				case o:PrefixOperation => {
    					w.write(o.operator)
    				}
    				case o:abs => {
    					w.write('|')
					}
    				case o:NamedOperation => {
    					w.write(o.operator)
    					if(!o.isInstanceOf[Operation1]){
    						writeOpeningBracket()
    					}
    				}
    				case _ =>
    			}
    		}
    		case s:Selection => {
    		    w.write("{")
    		}
            case ct:Case => {
                w.write("[")
            }
            case ct:CaseTest => {
                w.write("(")
            }
            case u:UnitOfValue => {
            	w.write("\u2009")
            	w.write(u.toNameString)
            	if(u.dimension!=1){
            		u.dimension match {
            			case 2 => w.write("²")
            			case 3 => w.write("³")
            			case _ => w.write(""+dimensionFormat.format(u.dimension))
            		}
            	}
            }
    		case _ => Unit
	    }
	}
	
	override def onBeforeChildEnter(node:Node[Expression], position:Int, child:Expression):Unit = node.element match {
		case _ => Unit
	}
	
	override def onBetweenChildren(node:Node[Expression], leftChild:Expression, rightChild:Expression):Unit = node.element match {
		case o:MultipleInfixOperation => {
            writeSpace()
            w.write(o.operator)
            writeSpace()
        }
	    case o:InfixOperation => {
			writeSpace()
			w.write(o.operator)
			writeSpace()
		}
		case o:OperationN => {
			writeListSeparator()
		}
        case s:Selection => {
            w.write(" or ")
        }
        case ct:Case => {
            w.write(" if ")
        }
        case ct:CaseTest => {
            
        }
		case _ => Unit
	}
	
	override def onAfterChildExit(node:Node[Expression], position:Int, child:Expression):Unit = node.element match {
		case _ => Unit
	}
	
	override def onExit(node:Node[Expression]) = {
		node.element match {
			case o:Operation => {
				o match {
					case o:PostfixOperation => {
						w.write(o.operator)
					}
    				case o:abs => {
    					w.write('|')
					}
					case o:NamedOperation => {
						if(!o.isInstanceOf[Operation1]){
							writeClosingBracket()
						}
					}
					case _ =>
				}
				writeClosingBracketIfNeeded(node,o)
			}
            case s:Selection => {
                w.write("}")
            }
            case ct:Case => {
                w.write("]")
            }
            case ct:CaseTest => {
                w.write(ct.operator.last)
                w.write(")")
            }
            case ev:EvalAt => {
                w.write("(")
                val s = ev.er.listMappings
                if(s.size<=5){
                    var ic = 0
                    s.foreach(
                        x => {
                           if(ic>0) w.write(", ")
                           x._1.visit(visitor = this)
                           w.write("=")
                           ev.er.evaluate(x._2)(new ResultsCache()).visit(visitor = this)
                           ic = ic + 1
                        }
                    )
                }
                w.write(")")
            }
			case _ => Unit
		}
		if(node.parent == null){
			w.flush()
		}
	}
	
}


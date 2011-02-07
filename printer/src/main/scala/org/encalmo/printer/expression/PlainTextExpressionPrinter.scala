package org.encalmo.printer.expression

import org.encalmo.printer._
import org.encalmo.expression._
import org.encalmo.common._

/**
 * Prints expressions as plain text 
 * @author artur.opala
 */
object PlainTextExpressionPrinter extends TextExpressionPrinter {
	
	override def print(e:Expression,output:TextOutput = new TextOutput):TextOutput = {
		val t = new PlainTextExpressionPrinterTraveler(output)
		e.travel(traveler = t)
		output
	}

}

/**
 * Simple Traveler printing expression as plain text
 * @author artur.opala
 */
class PlainTextExpressionPrinterTraveler(output:TextOutput) extends Traveler[Expression] {
	
	val w = output.asWriter
	val locale = output.locale
	
	lazy val integerFormat1:java.text.NumberFormat = new java.text.DecimalFormat("###,###,###,###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat1:java.text.NumberFormat = new java.text.DecimalFormat(".#",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat2:java.text.NumberFormat = new java.text.DecimalFormat(".##",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat3:java.text.NumberFormat = new java.text.DecimalFormat(".###",java.text.DecimalFormatSymbols.getInstance(locale))
	lazy val fractionFormat4:java.text.NumberFormat = new java.text.DecimalFormat(".####",java.text.DecimalFormatSymbols.getInstance(locale))
	
	def writeOpeningBracket = w.write('(');
	def writeClosingBracket = w.write(')');
	def writeSpace = w.write(' ');
	def writeSymbol(s:Symbol) = {
		s match {
			case sl:SymbolLocalized => w.write(output.translate(s.face2))
			case _ => w.write(s.face2)
		}
	}
	def writeNumber(n:Number) = {
		val nf:NumberFormatted = n.formatForPrint
		if (nf.isNegative)w.write("-");
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
	def writeListSeparator = w.write(';');
	
	def writeOpeningBracketIfNeeded(node:Node[Expression],o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeOpeningBracket
		}
	}
	
	def writeClosingBracketIfNeeded(node:Node[Expression],o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeClosingBracket
		}
	}
	
	def isBracketNeeded(node:Node[Expression],o:Operation):Boolean = {
		if(node.parent!=null){
			node.parent.element match {
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
    		case s:Symbol => writeSymbol(s)
    		case sl: SymbolLike => writeSymbol(sl.symbol)
    		case n:Number => writeNumber(n)
    		case tv: TextValue => w.write(tv.text)
    		case o:Operation => {
    			writeOpeningBracketIfNeeded(node,o)
    			o match {
    				case o:PrefixOperation => {
    					w.write(o.operator)
    				}
    				case o:NamedOperation => {
    					w.write(o.operator)
    					if(!o.isInstanceOf[Operation1]){
    						writeOpeningBracket
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
    		case _ => Unit
	    }
	}
	
	override def onBeforeChildEnter(node:Node[Expression], position:Int, child:Expression):Unit = node.element match {
		case _ => Unit
	}
	
	override def onBetweenChildren(node:Node[Expression], leftChild:Expression, rightChild:Expression):Unit = node.element match {
		case o:MultipleInfixOperation => {
            writeSpace
            w.write(o.operator)
            writeSpace
        }
	    case o:InfixOperation => {
			writeSpace
			w.write(o.operator)
			writeSpace
		}
		case o:OperationN => {
			writeListSeparator
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
					case o:NamedOperation => {
						if(!o.isInstanceOf[Operation1]){
							writeClosingBracket
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
			case _ => Unit
		}
		if(node.parent == null){
			w.flush()
		}
	}
	
}


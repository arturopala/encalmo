package org.encalmo.printer.expression

import org.encalmo.common._
import org.encalmo.printer._
import org.encalmo.expression._
import MathMLTags._

/**
 * Prints expressions as MathML
 * @author artur.opala
 */
object MathMLExpressionPrinter extends ExpressionPrinter[MathMLOutput, String] {

	override def print(e: Expression, output: MathMLOutput = new MathMLOutput): MathMLOutput = {
		val t = new MathMLExpressionPrinterTraveler(output)
		e.travel(traveler = t)
		output
	}

}

/**
 * Simple Traveler printing expression as MathML xml text
 * @author artur.opala
 */
class MathMLExpressionPrinterTraveler(output: MathMLOutput) extends Traveler[Expression] {
	
	val locale = output.locale

	def writeOpeningBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.leftBracket
		}
	}

	def writeClosingBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.rightBracket
		}
	}

	def isBracketNeeded(node: Node[Expression], o: Operation): Boolean = {
		if (node.parent != null) {
			node.parent.element match {
				case o:Quot => false
				case o:Power => false
				case o:sqrt => false
				case o:cbrt => false
				case o:root => false
				case po: Operation => po.precedence > o.precedence && (node.position > 0 || po.precedence - o.precedence > 5)
				case _ => false
			}
		} else {
			false
		}
	}

	// Traveler interface implementation

	override def onEnter(node: Node[Expression]): Unit = node.element match {
		case s: Symbol => output.symbol(s)
		case n: Number => output.mn(n,locale)
		case o: Operation => {
			writeOpeningBracketIfNeeded(node, o)
			o match {
				case o: PrefixOperation => {
					output.mo(o.operator,"prefix")
				}
				case o:Quot => {
					output.start(MFRAC)
					output.attr("linethickness","0.6")
					output.body
				}
				case o:Power => {
					output.startb(MSUP)
				}
				case o:sqrt => {
					output.startb(MSQRT)
				}
				case o:cbrt => {
					output.startb(MROOT)
				}
				case o:root => {
					output.startb(MROOT)
				}
				case o: NamedOperation => {
					output.mi(o.operator)
					if (!o.isInstanceOf[Operation1]) {
						output.leftBracket
					}
				}
				case _ => Unit
			}
		}
		case _ => Unit
	}

	override def onBeforeChildEnter(node: Node[Expression], position: Int, child: Expression): Unit = {
		child match {
			case o: Operation => {
				node.element match {
					case o:Power => {
						if (position==1) {
							output.startb(MROW)
						}
					}
					case o: Operation => {
						output.startb(MROW)
					}
					case _ => Unit
				}
			}
			case _ => Unit
		}
	}

	override def onBetweenChildren(node: Node[Expression], leftChild: Expression, rightChild: Expression): Unit = {
		node.element match {
			case o:Quot => Unit
			case o:Power => Unit
			case o:cbrt => Unit
			case o:root => Unit
			case o: InfixOperation => {
				output.mo(o.operator,"infix","thickmathspace","thickmathspace")
			}
			case o: Operation2 => {
				output.separator
			}
			case o: OperationN => {
				output.separator
			}
			case _ => Unit
		}
	}

	override def onAfterChildExit(node: Node[Expression], position: Int, child: Expression): Unit = {
		child match {
			case o: Operation => {
				node.element match {
					case o:Power => {
						if (position==1) {
							output.end(MROW)
						}
					}  
					case o:cbrt => {
						output.end(MROW)
						output.startb(MROW)
						output.startb(MN)
						output.append("3")
						output.end(MN)
						output.end(MROW)
					}
					case o: Operation => {
						output.end(MROW)
					}
					case _ => Unit
				}
			}
			case _ => Unit
		}
	}

	override def onExit(node: Node[Expression]) = {
		node.element match {
		case o: Operation => {
			o match {
				case o: PostfixOperation => {
					output.mo(o.operator,"postfix")
				}
				case o:Quot => {
					output.end(MFRAC)
				}
				case o:Power => {
					output.end(MSUP)
				}
				case o:sqrt => {
					output.end(MSQRT)
				}
				case o:cbrt => {
					output.end(MROOT)
				}
				case o:root => {
					output.end(MROOT)
				}
				case o: NamedOperation => {
					if (!o.isInstanceOf[Operation1]) {
						output.rightBracket
					}
				}
				case _ => Unit
			}
			writeClosingBracketIfNeeded(node, o)
		}
		case _ => Unit
		}
	}

}
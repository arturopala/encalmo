package org.encalmo.printer

import org.encalmo.expression._

/**
 * Prints expressions as MathML
 * @author artur.opala
 */
object MathMLExpressionPrinter extends ExpressionPrinter[MathMLOutput, String] {

	override def print(e: Expression, output: MathMLOutput = new MathMLOutput): MathMLOutput = {
		val t = new MathMLExpressionPrinterTraveler(output.asWriter, output.locale)
		e.travel(t = t)
		output
	}

}

/** 
 * MathML utilities
 * @author artur.opala
 */
object MathMLUtils {

	import java.io.Writer

	def thickspace(w:Writer):Unit = w write """<mspace width="thickmathspace"/>"""

	def thinspace(w:Writer):Unit = w write """<mspace width="thinmathspace"/>"""

	def leftBracket(w:Writer):Unit = w write """
	<mfenced open="(" close=")" separators=";">
	<mrow>"""

	def rightBracket(w:Writer):Unit = w write """</mrow>
	</mfenced>
	"""
	
	def separator(w:Writer):Unit = w write """</mrow><mrow>"""

	def mo(w: Writer, s: String): Unit = mo(w, s, null)

	def mo(w: Writer, s: String, form: String): Unit = mo(w, s, form, null, null)

	def mo(w: Writer, s: String, form: String, lspace: String, rspace: String): Unit = {
		w write "<mo"
		if (form != null) {
			w write " form=\""
			w write form
			w write "\""
		}
		if (lspace != null) {
			w write " lspace=\""
			w write lspace
			w write "\""
		}
		if (rspace != null) {
			w write " rspace=\""
			w write rspace
			w write "\""
		}
		w write ">"
		w write (s match {
		case "-" => "&minus;"
		case "+" => "+"
		case "*" => "&CenterDot;"
		case _ => s
		})
		w write "</mo>"
	}

	def mn(w: Writer, r: Real):Unit = {
		w write "<mrow>"
		if (r < 0) mo(w, "-", "prefix")
		val f = r.abs.format
		w write "<mn>"
		w write f
		w write "</mn>"
		/*if (f._2 != null) {
			w write "<mo>"
			w write "&CenterDot;"
			w write "</mo>"
			w write "<msup>"
			w write "<mn>"
			w write "10"
			w write "</mn>"
			w write "<mrow>"
			w write "<mn>"
			w write f._2
			w write "</mn>"
			w write "</mrow>"
			w write "</msup>"
		}*/
		w write "</mrow>"
	}

	def mi(w:Writer,s:String):Unit = {
		w write "<mi>"
		w write s
		w write "</mi>"
	}

	def mi(w: Writer, s: String, size: Int):Unit = {
		w write "<mi mathsize=\""
		w write "" + size
		w write "%\">"
		w write s
		w write "</mi>"
	}

	def mtext(w: Writer, s: String, size: Int):Unit = {
		w write "<mtext mathsize=\""
		w write "" + size
		w write "%\">"
		w write s
		w write "</mtext>"
	}

	def mrowstart(w: Writer):Unit = w write "\r\n<mrow>"

	def mrowend(w: Writer):Unit = w write "</mrow>\r\n"
	
	def symbol(w: Writer, s: Symbol):Unit = {
		//under-over script start
		if (s.hasOverAndUnderscript) w write "<munderover>"
		else if (s.hasOverscript) w write "<mover>"
		else if (s.hasUnderscript) w write "<munder>"
		if (s.hasOverAndUnderscript && s.hasSubAndSupscript) w write "<mrow>"
		//sub-super script start
		if (s.hasSubAndSupscript) w write "<msubsup>"
		else if (s.hasSuperscript) w write "<msup>"
		else if (s.hasSubscript) w write "<msub>"
		//core symbol
		mi(w, BasicSymbols.toMathML(s.name))
		if (s.hasSubscript) symbol(w, s.subscript)
		if (s.hasSuperscript) symbol(w, s.superscript)
		if (s.hasSubAndSupscript) w write "</msubsup>"
		else if (s.hasSuperscript) w write "</msup>"
		else if (s.hasSubscript) w write "</msub>"
		//sub-super script end
		if (s.hasOverAndUnderscript && s.hasSubAndSupscript) w write "</mrow>"
		if (s.hasUnderscript) symbol(w, s.underscript)
		if (s.hasOverscript) symbol(w, s.overscript)
		if (s.hasOverAndUnderscript) w write "</munderover>"
		else if (s.hasOverscript) w write "</mover>"
		else if (s.hasUnderscript) w write "</munder>"
		//under-over script end
	}

}

/**
 * Simple Traveler printing expression as MathML xml text
 * @author artur.opala
 */
class MathMLExpressionPrinterTraveler(w: java.io.Writer, locale: java.util.Locale = java.util.Locale.getDefault) extends Traveler {

	import MathMLUtils._

	def writeOpeningBracketIfNeeded(node: Node, o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			leftBracket(w)
		}
	}

	def writeClosingBracketIfNeeded(node: Node, o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			rightBracket(w)
		}
	}

	def isBracketNeeded(node: Node, o: Operation): Boolean = {
		if (node.parent != null) {
			node.parent.expr match {
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

	override def onEnter(node: Node): Unit = node.expr match {
		case s: Symbol => symbol(w,s)
		case n: Number => mn(w,n.r)
		case o: Operation => {
			writeOpeningBracketIfNeeded(node, o)
			o match {
				case o: PrefixOperation => {
					mo(w,o.operator,"prefix")
				}
				case o:Quot => {
					w write "<mfrac linethickness=\"0.8\">"
				}
				case o:Power => {
					w write "<msup>"
				}
				case o:sqrt => {
					w write "<msqrt>"
				}
				case o:cbrt => {
					w write "<mroot>"
				}
				case o:root => {
					w write "<mroot>"
				}
				case o: NamedOperation => {
					w.write(o.operator)
					if (!o.isInstanceOf[Operation1]) {
						leftBracket(w)
					}
				}
				case _ => Unit
			}
		}
		case _ => Unit
	}

	override def onBeforeChildEnter(node: Node, position: Int, child: Expression): Unit = node.expr match {
		case o:Power => {
			if (position==1) {
				mrowstart(w)
			}
		}
		case o: Operation => {
			mrowstart(w)
		}
		case _ => Unit
	}

	override def onBetweenChildren(node: Node, leftChild: Expression, rightChild: Expression): Unit = node.expr match {
		case o:Quot => Unit
		case o:Power => Unit
		case o:cbrt => Unit
		case o:root => Unit
		case o: InfixOperation => {
			mo(w,o.operator,"infix","thickmathspace","thickmathspace")
		}
		case o: Operation2 => {
			separator(w)
		}
		case o: OperationN => {
			separator(w)
		}
		case _ => Unit
	}

	override def onAfterChildExit(node: Node, position: Int, child: Expression): Unit = node.expr match {
		case o:Power => {
			if (position==1) {
				mrowend(w)
			}
		}  
		case o:cbrt => {
			w write "</mrow><mrow><mn>3</mn></mrow>"
		}
		case o: Operation => {
			mrowend(w)
		}
		case _ => Unit
	}

	override def onExit(node: Node) = {
		node.expr match {
		case o: Operation => {
			o match {
				case o: PostfixOperation => {
					mo(w,o.operator,"postfix")
				}
				case o:Quot => {
					w write "</mfrac>"
				}
				case o:Power => {
					w write "</msup>"
				}
				case o:sqrt => {
					w write "</msqrt>"
				}
				case o:cbrt => {
					w write "</mroot>"
				}
				case o:root => {
					w write "</mroot>"
				}
				case o: NamedOperation => {
					if (!o.isInstanceOf[Operation1]) {
						rightBracket(w)
					}
				}
				case _ => Unit
			}
			writeClosingBracketIfNeeded(node, o)
		}
		case _ => Unit
		}
		if (node.parent == null) {
			w.flush()
		}
	}

}
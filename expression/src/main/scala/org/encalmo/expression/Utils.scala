package org.encalmo.expression

/**
 * Simple Traveler printing expression as plain text
 * @author artur.opala
 */
class PlainTextExpressionPrinterTraveler(w:java.io.Writer, locale:java.util.Locale = java.util.Locale.getDefault) extends Traveler {
	
	def writeOpeningBracket = w.write('(');
	def writeClosingBracket = w.write(')');
	def writeSpace = w.write(' ');
	def writeSymbol(s:Symbol) = w.write(s.face) 
	def writeNumber(n:Number) = w.write(n.format(locale))
	def writeListSeparator = w.write(';');
	
	def writeOpeningBracketIfNeeded(node:Node,o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeOpeningBracket
		}
	}
	
	def writeClosingBracketIfNeeded(node:Node,o:Operation):Unit = {
		if(isBracketNeeded(node,o)){
			writeClosingBracket
		}
	}
	
	def isBracketNeeded(node:Node,o:Operation):Boolean = {
		if(node.parent!=null){
			node.parent.expr match {
				case po:Operation => po.precedence>o.precedence && (node.position>0 || po.precedence-o.precedence>5)
				case _ => false
			}
		}else{
			false
		}
	}
	
	override def onEnter(node:Node):Unit = node.expr match {
		case s:Symbol => writeSymbol(s)
		case n:Number => writeNumber(n)
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
		case _ => Unit
	}
	
	override def onBeforeChildEnter(node:Node, position:Int, child:Expression):Unit = node.expr match {
		case _ => Unit
	}
	
	override def onBetweenChildren(node:Node, leftChild:Expression, rightChild:Expression):Unit = node.expr match {
		case o:InfixOperation => {
			writeSpace
			w.write(o.operator)
			writeSpace
		}
		case o:OperationN => {
			writeListSeparator
		}
		case _ => Unit
	}
	
	override def onAfterChildExit(node:Node, position:Int, child:Expression):Unit = node.expr match {
		case _ => Unit
	}
	
	override def onExit(node:Node) = {
		node.expr match {
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
			case _ => Unit
		}
		if(node.parent == null){
			w.flush()
		}
	}
	
}
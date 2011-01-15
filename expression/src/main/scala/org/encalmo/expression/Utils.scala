package org.encalmo.expression

import org.encalmo.common._

/**
 * Simple Traveler printing expression as plain text
 * @author artur.opala
 */
class PlainTextExpressionPrinterTraveler(w:java.io.Writer, locale:java.util.Locale = java.util.Locale.getDefault) extends Traveler[Expression] {
	
	def writeOpeningBracket = w.write('(');
	def writeClosingBracket = w.write(')');
	def writeSpace = w.write(' ');
	def writeSymbol(s:Symbol) = w.write(s.face) 
	def writeNumber(n:Number) = w.write(n.format(locale))
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
	
	override def onEnter(node:Node[Expression]):Unit = node.element match {
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
	
	override def onBeforeChildEnter(node:Node[Expression], position:Int, child:Expression):Unit = node.element match {
		case _ => Unit
	}
	
	override def onBetweenChildren(node:Node[Expression], leftChild:Expression, rightChild:Expression):Unit = node.element match {
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
			case _ => Unit
		}
		if(node.parent == null){
			w.flush()
		}
	}
	
}
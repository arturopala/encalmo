package org.encalmo.document
import org.encalmo.style.Style

/**
 * Character component class
 */
class Character(customStyle: Option[Style], val text:String)
extends DocumentComponent(customStyle)

/**
 * Character class companion object
 * @author artur.opala
 */
object Character{
	
	def apply(customStyle:Style, text:String) = {
		new Character(Option(customStyle),text)
	}
	
	def apply(text:String) = {
		new Character(None,text)
	}
	
	def unapply(t:Character) = Some((t.customStyle,t.text))
	
	/** space */
	val SPACE = Character(" ")
	/** long space */
    val LONGSPACE = Character("  ")
    /** equal character */
    val EQUAL = Character("=")
    /** lower character */
    val LOWER = Character("<")
    /** greater character */
    val GREATER = Character(">")
	/** lower or equal character */
	val LE = Character("≤")
	/** greater or equal character */
    val GE = Character("≥")
	/** right arrow character */
    val RARROW = Character("→")
	
}
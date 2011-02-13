package org.encalmo.document

/**
 * Character component class
 */
class Character(myStyle:Style, val text:String) 
extends DocumentComponent(myStyle)

/**
 * Character class companion object
 * @author artur.opala
 */
object Character{
	
	def apply(mystyle:Style, text:String) = {
		new Character(mystyle,text)
	}
	
	def apply(text:String) = {
		new Character(null,text)
	}
	
	def unapply(t:Character) = Some((t.myStyle,t.text))
	
	/** space */
	val SPACE = Character(" ")
	/** long space */
    val LONGSPACE = Character("  ")
	/** lower or equal character */
	val LE = Character("≤")
	/** greater or equal character */
    val GE = Character("≥")
	/** right arrow character */
    val RARROW = Character("→")
	
}
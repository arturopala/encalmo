package org.encalmo.document

import org.encalmo.expression.Expression
import org.encalmo.style.Style
import org.encalmo.calculation.{Context, ContextFactory}

/**
 * Document component representing symbol or expression
 * @author artur.opala
 */
case class Symb(override val customStyleOfComponent: Option[Style], expression:Expression)
extends DocumentComponent(customStyleOfComponent)

object Symb {
	
	def apply(e: Expression): Symb = {
		new Symb(None,e)
	}
	
	def apply(customStyle: Style, e: Expression): Symb = {
		new Symb(Option(customStyle),e)
	}
	
}
package org.encalmo

import scala.language.implicitConversions
import org.encalmo.expression.Expression

package object document {
	
	implicit def string2Text(s:String):DocumentComponent = Text(s)
	implicit def enumerator2DocumentComponent(en:Enumerator):DocumentComponent = en.toDocumentComponent
    implicit def expression2DocumentComponent(expression: Expression):DocumentComponent = Symb(expression)
	
}
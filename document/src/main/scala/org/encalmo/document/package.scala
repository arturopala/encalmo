package org.encalmo

import scala.language.implicitConversions

package object document {
	
	implicit def string2Text(s:String):DocumentComponent = Text(s)
	
	implicit def enumerator2DocumentComponent(en:Enumerator):DocumentComponent = en.toDocumentComponent
	
}
package org.encalmo

package object document {
	
	implicit def string2Text(s:String):DocumentComponent = Text(s)
	
}
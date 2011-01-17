package org.encalmo.printer

/**
 * LayoutFormat class, all dimensions in [mm]
 * @author artur.opala
 */
class LayoutFormat(
	val width:Int,
	val height:Int
)

/**
 * Predefined layout formats
 * @author artur.opala
 */
object LayoutFormat {
	
	lazy val A5 = new LayoutFormat(147,210)
	lazy val A4 = new LayoutFormat(210,297)
	lazy val A3 = new LayoutFormat(297,420)
	
}
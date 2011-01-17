package org.encalmo.printer

/**
 * PrintLayout class, , all dimensions in [mm]
 * @author artur.opala
 */
case class Layout(
	id:String = "default",
	format:LayoutFormat = LayoutFormat.A4,
	orientation:LayoutOrientation = LayoutOrientation.PORTRAIT,
	leftMargin:Int = 20,
	topMargin:Int = 20,
	rightMargin:Int = 20,
	bottomMargin:Int = 20
)
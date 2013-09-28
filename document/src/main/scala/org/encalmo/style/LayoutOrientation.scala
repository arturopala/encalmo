package org.encalmo.style

/**
 * LayoutOrientation class
 * @author artur.opala
 */
sealed class LayoutOrientation

object LayoutOrientation {
	
	lazy val PORTRAIT = new LayoutOrientation
	lazy val LANDSCAPE = new LayoutOrientation
	
}
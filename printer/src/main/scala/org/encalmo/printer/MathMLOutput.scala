package org.encalmo.printer

/**
 * MathML printer output
 * @author artur.opala
 */
class MathMLOutput extends TextOutput {
	
	override def open = {
		this +\ """<math xmlns="http://www.w3.org/1998/Math/MathML" mode="inline">"""
		this +\ """<mrow>"""
	}
	
	override def close = {
		this \+ """</mrow>""" \+ """</math>"""
	}
	
}
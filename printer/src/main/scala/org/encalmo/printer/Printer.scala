package org.encalmo.printer

/**
 * General printer trait
 * A - output type
 * B - content type
 * C - input type
 * D - context type
 * @author artur.opala
 */
trait Printer[C,A<:Output[B],B, D] {
	
	def print(input:C)(output:A)(context: D):A
	
}
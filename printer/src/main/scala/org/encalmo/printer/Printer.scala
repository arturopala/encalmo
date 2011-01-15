package org.encalmo.printer

/**
 * General printer trait
 * A - output type
 * B - result type
 * C - input type
 * @author artur.opala
 */
trait Printer[C,A<:Output[B],B] {
	
	def print(input:C,output:A):A
	
}
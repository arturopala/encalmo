package org.encalmo.expression

/**
 * Formatted number container
 * @author artur.opala
 */
case class NumberFormatted(
	isNegative:Boolean,
	hasExponent:Boolean,
	integer:Long,
	fraction:Double,
	exponent:Int,
	decimals:Int,
	unit:UnitOfValue
)
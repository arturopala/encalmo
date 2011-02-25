package org.encalmo.expression

/**
 * CannotSumUnitsException. Throwed when cyclic reference in the tree of A is detected
 */
class CannotSumUnitsException(unit1:UnitOfValue,unit2:UnitOfValue) extends RuntimeException
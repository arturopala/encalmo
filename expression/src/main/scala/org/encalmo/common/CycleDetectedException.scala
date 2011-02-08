package org.encalmo.common

/**
 * CycleDetectedException. Throwed when cyclic reference in the tree of A is detected
 */
class CycleDetectedException[A](source:A,message: String) extends RuntimeException(message)
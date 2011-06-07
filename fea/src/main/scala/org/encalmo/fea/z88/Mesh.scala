package org.encalmo.fea.z88

/**
 * Mesh trait
 */
trait Mesh[A <: FiniteElement] {
    
    /** Finite elements sequence */
    def elements:Seq[A]
    /** First element */
    private val e:A = elements.first
    /** Dimension of the structure (2 or 3) */
    def dimension:Int = e.attr.dimension
    /** Number of finite elements */
    def size = elements.size
    /** Number of degrees of freedom */
    def dof = size*e.attr.dof
    /** Number of material information lines */
    def matlines = matgroups.size
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG = e.attr.IBFLAG
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG = e.attr.IPFLAG
    /** Coordinate flag KFLAG (0 or 1) */
    def KFLAG:Int
    /** Surface and pressure loads flag IQFLAG (0 or 1) */
    def IQFLAG:Int
    
    /** Elements grouped by their materials */
    lazy val matgroups:Map[Material,Seq[A]] = elements.groupBy(e => e.material)
    
}

/** 
 * Mesh factory
 */
object Mesh {
    
    def apply[A <: FiniteElement](elements:Seq[A]):Mesh[A] = new MeshImpl[A](elements);
    
}

/**
 * Base mesh impl
 */
class MeshImpl[A <: FiniteElement](
        
        /** Finite elements sequence */
        override val elements:Seq[A],
        /** Coordinate flag KFLAG (0 or 1) */
        override val KFLAG:Int = 0,
        /** Surface and pressure loads flag IQFLAG (0 or 1) */
        override val IQFLAG:Int = 0
        
) extends Mesh[A] {
    
}


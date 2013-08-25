package org.encalmo.fea

/**
 * Mesh trait
 */
trait Mesh[A <: FiniteElement] {
    
    /** Finite elements sequence */
    def elements:Seq[A]
    /** First element */
    val attr:FiniteElementType = elements.head.attr
    /** Dimension of the structure (2 or 3) */
    def dimension:Int = attr.dimension
    /** Number of finite elements */
    def size:Int = elements.size
    /** Number of degrees of freedom */
    def dof:Int = nodes.size*attr.dof
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG:Int = attr.IBFLAG
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG:Int = attr.IPFLAG
    /** Coordinate flag KFLAG (0 or 1) */
    def KFLAG:Int
    /** Surface and pressure loads flag IQFLAG (0 or 1) */
    def IQFLAG:Int 
    
    /** All nodes sequence */
    val nodes:Seq[Node] = {
        val ns:Seq[Node] = elements.flatMap(_.nodes).toSet.toSeq.sorted
        Mesh.renumber(ns)
        Mesh.renumber(elements.sorted[FiniteElement])
        ns
    }
    
}

/** 
 * Mesh factory
 */
object Mesh {
    
    /** New mesh from sequence of finite elements */
    def apply[A <: FiniteElement](elements:Seq[A]):Mesh[A] = new MeshImpl[A](elements)

    /** Utility function: assigns numbers */
    def renumber(s:Seq[Numbered]) = s.foldLeft[Int](0)((p,a) => {a.no = p + 1; a.no})
    
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


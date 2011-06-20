package org.encalmo.fea.z88

/**
 * Mesh trait
 */
trait Mesh[A <: FiniteElement] {
    
    /** Finite elements sequence */
    def elements:Seq[A]
    /** First element */
    val e:A = elements.first
    /** Dimension of the structure (2 or 3) */
    def dimension:Int = e.attr.dimension
    /** Number of finite elements */
    def size:Int = elements.size
    /** Number of degrees of freedom */
    def dof:Int = nodes.size*e.attr.dof
    /** Number of material information lines */
    def matlines:Int = matgroups.size
    /** Beam flag IBFLAG (0 or 1) */
    def IBFLAG:Int = e.attr.IBFLAG
    /** Plate flag IPFLAG (0 or 1) */
    def IPFLAG:Int = e.attr.IPFLAG
    /** Coordinate flag KFLAG (0 or 1) */
    def KFLAG:Int
    /** Surface and pressure loads flag IQFLAG (0 or 1) */
    def IQFLAG:Int 
    
    /** All nodes sequence */
    val nodes:Seq[Node] = {
        val ns = elements.flatMap(_.nodes).toSet.toSeq.sorted
        Mesh.renumber(ns)
        Mesh.renumber(elements.sorted[FiniteElement])
        ns
    }
    /** Map of elements grouped by materials */
    lazy val matgroups:Seq[(Int, Seq[Any], Seq[A])] = {
        var c = 0
        elements.sorted[FiniteElement]
         .groupBy(_.matinfo)
         .flatMap(e => {val s = groupStrictAscending(c,e._1,e._2); c=c+s.size; s})
         .toSeq.sortWith((l,r) => (l._3.head.no<r._3.head.no))
    }
    
    private def groupStrictAscending(cpos:Int,info:Seq[Any],elems:Seq[A]):Seq[(Int, Seq[Any], Seq[A])]  = {
        var p = elems.head; var i = cpos
        elems.groupBy(e1 => { if(e1.no-p.no>1) i = i+1; p = e1; i}).map(e2 => (e2._1,info,e2._2)).toSeq
    }
    
}

/** 
 * Mesh factory
 */
object Mesh {
    
    /** New mesh from sequence of finite elements */
    def apply[A <: FiniteElement](elements:Seq[A]):Mesh[A] = new MeshImpl[A](elements);
    
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


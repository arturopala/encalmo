package org.encalmo.fea

/** Mesh load and boundary conditions case */
case class LoadCase[A <: FiniteElement](
        /** Mesh under load and boundary conditions */
        mesh:Mesh[A],
        /** Material properties function */
        materialFx:A=>Material,
        /** Thickness function */
        thicknessFx:A=>Double,
        /** Surface load function */
        surfaceLoadFx:A=>OptDoubleSeq,
        /** Nodal forces function */
        nodalForceFx:Node=>OptDoubleSeq,
        /** Nodal displacement function */
        nodalDisplacementFx:Node=>OptDoubleSeq
        
) {
    /** Ordered mesh elements */
    lazy val elements = mesh.elements.sorted[FiniteElement]
    /** All mesh nodes */
    lazy val nodes = mesh.nodes
    
    /** Map of element's cases */
    lazy val elementCases:Seq[ElementCase[A]] = {
        elements.map(e => ElementCase[A](e,materialFx(e),thicknessFx(e),surfaceLoadFx(e)))
    }
    /** Map of node's cases */
    lazy val nodeCases:Seq[NodeCase] = {
        nodes.map(n => NodeCaseOption(n,nodalDisplacementFx(n),nodalForceFx(n)))
             .filter(_.isDefined) // skip None mappings
             .map(_.get) // extract value from Some
             .sortWith((l,r) => l.node.no<r.node.no) // sort ascending
    }
    
    /** Number of material information lines */
    def matlines:Int = matgroups.size
    
    /** Map of elements grouped by materials */
    lazy val matgroups:Seq[(Int, Seq[Any], Seq[A])] = {
        var c = 0
        val s1 = elementCases.groupBy(_.matinfo).map(e => (e._1,e._2.map(_.element).toSeq.sorted[FiniteElement]))
        val s2 = s1.flatMap(e => {val s = groupStrictAscending(c,e._1,e._2); c=c+s.size; s})
        val s3 = s2.toSeq.sortWith((l,r) => (l._3.head.no<r._3.head.no))
        s3
    }
    /** Helper function */
    private def groupStrictAscending(cpos:Int,info:Seq[Any],elems:Seq[A]):Seq[(Int, Seq[Any], Seq[A])]  = {
        var p = elems.head; var i = cpos
        elems.groupBy(e1 => { if(e1.no-p.no>1) i = i+1; p = e1; i}).map(e2 => (e2._1,info,e2._2)).toSeq
    }

}

/** Handy OptDoubleSeq factory */
object OptDoubleSeq {
    
    def apply(d:Double*):OptDoubleSeq = Some(Seq(d:_*).map(Option(_)))
    
}
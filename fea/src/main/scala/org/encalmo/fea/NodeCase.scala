package org.encalmo.fea

/** Nodal forces and displacements */
case class NodeCase(
        /** Referenced node */
        node:Node,
        /** Node displacement case, data size depends on node's DOF */
        displacement:OptDoubleSeq = None,
        /** Nodal forces case, data size depends on node's DOF */
        force:OptDoubleSeq = None
) {

}

object NodeCaseOption {
    
    def apply(node:Node,displacement:OptDoubleSeq = None,force:OptDoubleSeq = None):Option[NodeCase] = {
        if((!displacement.isDefined || allNone(displacement.get)) && (!force.isDefined || allNone(force.get))) {
            None
        } else {
            Some(NodeCase(node,displacement,force))
        }
    }
    
    def allNone(optseq:Seq[Option[Double]]):Boolean = optseq.forall(!_.isDefined)
    
}
package org.encalmo.fea

/** Nodal forces and displacements */
case class NodeConditions(
        /** Referenced node */
        node:Node,
        /** Node displacement case, data size depends on node's DOF */
        displacements:OptDoubleSeq = None,
        /** Nodal forces case, data size depends on node's DOF */
        forces:OptDoubleSeq = None
) {

}

object NodeConditionsOption {
    
    def apply(node:Node,displacement:OptDoubleSeq = None,force:OptDoubleSeq = None):Option[NodeConditions] = {
        if((!displacement.isDefined || allNone(displacement.get)) && (!force.isDefined || allNone(force.get))) {
            None
        } else {
            Some(NodeConditions(node,displacement,force))
        }
    }
    
    def allNone(optseq:Seq[Option[Double]]):Boolean = optseq.forall(!_.isDefined)
    
}
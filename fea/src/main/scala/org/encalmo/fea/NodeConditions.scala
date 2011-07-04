package org.encalmo.fea

/** Node's boundary conditions: force and displacement */
case class NodeConditions(
        /** Referenced node */
        node:Node,
        /** Established node displacement */
        displacement:Option[NodeDisplacement] = None,
        /** Established node force */
        forces:Option[NodeForce] = None
) {

}

object NodeConditionsOption {
    
    def apply(attr:FiniteElementType,node:Node,displacement:Option[NodeDisplacement] = None,force:Option[NodeForce] = None):Option[NodeConditions] = {
        if((!displacement.isDefined || !displacement.get.isDefined) && (!force.isDefined || !force.get.isDefined)) {
            None
        } else {
            Some(NodeConditions(node,displacement,force))
        }
    }
    
    def allNone(optseq:Seq[Option[Double]]):Boolean = optseq.forall(!_.isDefined)
    
}
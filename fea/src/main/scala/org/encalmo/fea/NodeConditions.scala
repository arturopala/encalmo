package org.encalmo.fea

/** Node's boundary conditions: force and displacement */
case class NodeConditions(
        /** Referenced node */
        node:Node,
        /** Established node displacement */
        displacement:Option[NodeDisplacement] = None,
        /** Established node force */
        force:Option[NodeForce] = None
) {
    
    /** Returns true if has any boundary condition */
    lazy val isDefined:Boolean = node!=null && ((displacement.isDefined && displacement.get.isDefined) || (force.isDefined && force.get.isDefined))
    /** Returns true if has boundary condition for dx */
    lazy val hasConditionForDX:Boolean = displacement.isDefined && displacement.get.dx.isDefined
    /** Returns true if has boundary condition for dy */
    lazy val hasConditionForDY:Boolean = displacement.isDefined && displacement.get.dy.isDefined
    /** Returns true if has boundary condition for dz */
    lazy val hasConditionForDZ:Boolean = displacement.isDefined && displacement.get.dz.isDefined
    /** Returns true if has boundary condition for rx */
    lazy val hasConditionForRX:Boolean = displacement.isDefined && displacement.get.rx.isDefined
    /** Returns true if has boundary condition for ry */
    lazy val hasConditionForRY:Boolean = displacement.isDefined && displacement.get.ry.isDefined
    /** Returns true if has boundary condition for rz */
    lazy val hasConditionForRZ:Boolean = displacement.isDefined && displacement.get.rz.isDefined

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
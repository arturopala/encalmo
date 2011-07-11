package org.encalmo.fea

/** Finite element node's results: displacement, stress, force */
case class NodeResult(
        /** Referenced node */
        node:Node,
        /** Boundary conditions (if any) */
        conditions:Option[NodeConditions],
        /** Displacement */
        displacement:NodeDisplacement,
        /** Stress */
        stress:Option[NodeStress],
        /** Force */
        force:NodeForce
) {
  
    /** Returns true if node and conditions are set */
    lazy val hasConditions:Boolean = node!=null && conditions.isDefined && conditions.get.isDefined
	/** Returns true if node, displacement and stress are set */
 	lazy val isComplete:Boolean = hasDisplacement && hasForce && hasStress
 	/** Returns true if node and displacement are set */
 	lazy val hasDisplacement:Boolean = node!=null && displacement!=null && displacement.isDefined
 	/** Returns true if node and stress are set */
 	lazy val hasStress:Boolean = node!=null && stress.isDefined && stress.get.isDefined
 	/** Returns true if node and force are set */
 	lazy val hasForce:Boolean = node!=null && force!=null && force.isDefined
 	/** Returns true if node and force are set, and node is located on the surface or inside solid */
    lazy val hasForceOnSurfaceOrInside:Boolean = hasForce && node.onSurfaceOrInside
    /** Returns true if node and force are set, and node is located on the edge or in the corner */
    lazy val hasForceOnEdgeOrInCorner:Boolean = hasForce && node.onEdgeOrInCorner
    
    /** Returns true if node and displacement are set, and has boundary condition for dx */
    lazy val hasDisplacementAndConditionForDX:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForDX
    /** Returns true if node and displacement are set, and  has boundary condition for dy */
    lazy val hasDisplacementAndConditionForDY:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForDY
    /** Returns true if node and displacement are set, and has boundary condition for dz */
    lazy val hasDisplacementAndConditionForDZ:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForDZ
    /** Returns true if node and displacement are set, and has boundary condition for rx */
    lazy val hasDisplacementAndConditionForRX:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForRX
    /** Returns true if node and displacement are set, and has boundary condition for ry */
    lazy val hasDisplacementAndConditionForRY:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForRY
    /** Returns true if node and displacement are set, and has boundary condition for rz */
    lazy val hasDisplacementAndConditionForRZ:Boolean = hasDisplacement && conditions.isDefined && conditions.get.hasConditionForRZ
    /** Returns true if node and displacement are set, and has boundary condition for dx */
    lazy val hasDisplacementAndNotConditionForDX:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForDX)
    /** Returns true if node and displacement are set, and  has boundary condition for dy */
    lazy val hasDisplacementAndNotConditionForDY:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForDY)
    /** Returns true if node and displacement are set, and has boundary condition for dz */
    lazy val hasDisplacementAndNotConditionForDZ:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForDZ)
    /** Returns true if node and displacement are set, and has boundary condition for rx */
    lazy val hasDisplacementAndNotConditionForRX:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForRX)
    /** Returns true if node and displacement are set, and has boundary condition for ry */
    lazy val hasDisplacementAndNotConditionForRY:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForRY)
    /** Returns true if node and displacement are set, and has boundary condition for rz */
    lazy val hasDisplacementAndNotConditionForRZ:Boolean = hasDisplacement && !(conditions.isDefined && conditions.get.hasConditionForRZ)
    
    /** Returns true if node and stress are set, and has boundary condition for dx */
    lazy val hasStressAndConditionForDX:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForDX
    /** Returns true if node and stress are set, and  has boundary condition for dy */
    lazy val hasStressAndConditionForDY:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForDY
    /** Returns true if node and stress are set, and has boundary condition for dz */
    lazy val hasStressAndConditionForDZ:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForDZ
    /** Returns true if node and stress are set, and has boundary condition for rx */
    lazy val hasStressAndConditionForRX:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForRX
    /** Returns true if node and stress are set, and has boundary condition for ry */
    lazy val hasStressAndConditionForRY:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForRY
    /** Returns true if node and stress are set, and has boundary condition for rz */
    lazy val hasStressAndConditionForRZ:Boolean = hasStress && conditions.isDefined && conditions.get.hasConditionForRZ
    /** Returns true if node and stress are set, and has boundary condition for dx */
    lazy val hasStressAndNotConditionForDX:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForDX)
    /** Returns true if node and stress are set, and  has boundary condition for dy */
    lazy val hasStressAndNotConditionForDY:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForDY)
    /** Returns true if node and stress are set, and has boundary condition for dz */
    lazy val hasStressAndNotConditionForDZ:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForDZ)
    /** Returns true if node and stress are set, and has boundary condition for rx */
    lazy val hasStressAndNotConditionForRX:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForRX)
    /** Returns true if node and stress are set, and has boundary condition for ry */
    lazy val hasStressAndNotConditionForRY:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForRY)
    /** Returns true if node and stress are set, and has boundary condition for rz */
    lazy val hasStressAndNotConditionForRZ:Boolean = hasStress && !(conditions.isDefined && conditions.get.hasConditionForRZ)
    
    /** Verbose node result explanation */
    def explain:String = "#"+node.no+" "+node.positionDescription+" "+node.coordinates.explain + 
        "\r\n\tdisplacement: "+displacement.explain + 
        "\r\n\tstress: "+stress.map(_.explain).getOrElse("unknown, not an element's corner node") + 
        "\r\n\tforce: "+force.explain
    
}
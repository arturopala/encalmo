package org.encalmo.fea

/** Finite element node displacement, force */
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
  
	/** Returns true if node, displacement and stress are set */
 	lazy val isComplete:Boolean = hasDisplacement && hasStress
 	
 	/** Returns true if node and displacement are set */
 	lazy val hasDisplacement:Boolean = node!=null && displacement!=null && displacement.isDefined
 	
 	/** Returns true if node and stress are set */
 	lazy val hasStress:Boolean = node!=null && stress.isDefined && stress.get.isDefined
 	
 	/** Returns true if node and force are set */
 	lazy val hasForce:Boolean = node!=null && force!=null && force.isDefined
    
    /** Verbose node result explanation */
    def explain:String = "#"+node.no+" "+node.positionDescription+" "+node.coordinates.explain + 
        "\r\n\tdisplacement: "+displacement.explain + 
        "\r\n\tstress: "+stress.map(_.explain).getOrElse("unknown, not an element's corner node") + 
        "\r\n\tforce: "+force.explain
    
}
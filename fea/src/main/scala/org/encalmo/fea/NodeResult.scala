package org.encalmo.fea

/** Finite element node displacement, force */
case class NodeResult(
        /** Referenced node */
        node:Node,
        /** Boundary conditions (if any) */
        conditions:Option[NodeConditions],
        /** Displacement */
        displacement:NodeDisplacement,
        /** Force */
        force:NodeForce
) {
    
    /** Verbose node result explanation */
    def explain:String = "#"+node.no+" "+node.positionDescription+" "+node.coordinates.explain + 
        "\r\n\tdisplacement: "+displacement.explain + 
        "\r\n\tforce: "+force.explain
    
}
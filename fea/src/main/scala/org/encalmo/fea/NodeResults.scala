package org.encalmo.fea

/** Finite element node displacements, stresses and forces */
case class NodeResults(
        /** Referenced node */
        node:Node,
        /** Boundary conditions (if any) */
        conditions:Option[NodeConditions],
        /** Displacements */
        displacements:OptDoubleSeq,
        /** Stresses */
        stresses:OptDoubleSeq,
        /** Agregated nodal forces */
        forces:OptDoubleSeq
) {
    
    lazy val D1 = D(1)
    lazy val D2 = D(2)
    lazy val D3 = D(3)
    lazy val D4 = D(4)
    lazy val D5 = D(5)
    lazy val D6 = D(6)
        
    private def D(i:Int) = displacements match {case Some(s) => s(i-1); case _ => None}
    
    def explain:String = "#"+node.no+" "+node.positionDescription+" "+node.coordinates+"\r\n\tdisplacement: "+displacements.getOrElse(Seq()).mkString("")
    
  
}
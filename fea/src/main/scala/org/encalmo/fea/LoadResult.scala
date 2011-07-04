package org.encalmo.fea

/** Mesh displacements, stresses and nodal forces */
case class LoadResults[A <: FiniteElement](
        /** Calculated load case */
        loadCase:LoadCase[A],
        /** Results at nodes */
        nodeResults:Seq[NodeResult] = Seq(),
        /** Results in elements */
        elementResults:Seq[ElementResult[A]] = Seq()
) {
    
    lazy val maxDX:NodeResult = findMax(_.displacement.dx)
    lazy val maxDY:NodeResult = findMax(_.displacement.dy)
    lazy val maxDZ:NodeResult = findMax(_.displacement.dz)
    lazy val maxRX:NodeResult = findMax(_.displacement.rx)
    lazy val maxRY:NodeResult = findMax(_.displacement.ry)
    lazy val maxRZ:NodeResult = findMax(_.displacement.rz)
    
    lazy val minDX:NodeResult = findMin(_.displacement.dx)
    lazy val minDY:NodeResult = findMin(_.displacement.dy)
    lazy val minDZ:NodeResult = findMin(_.displacement.dz)
    lazy val minRX:NodeResult = findMin(_.displacement.rx)
    lazy val minRY:NodeResult = findMin(_.displacement.ry)
    lazy val minRZ:NodeResult = findMin(_.displacement.rz)
       
    private def find(compare:(Double,Double)=>Boolean)(get:(NodeResult)=>Option[Double]) = 
        nodeResults.tail.foldLeft(nodeResults.head)((p,nr) => if(get(p).isDefined && get(nr).isDefined && compare(get(p).get,get(nr).get)) nr else p)

    private def findMax:((NodeResult)=>Option[Double])=>NodeResult = find((p,c) => c > p)
    
    private def findMin:((NodeResult)=>Option[Double])=>NodeResult = find((p,c) => c < p)

}
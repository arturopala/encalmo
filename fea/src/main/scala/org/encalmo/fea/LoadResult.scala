package org.encalmo.fea

/** Mesh displacements, stresses and nodal forces */
case class LoadResults[A <: FiniteElement](
        /** Calculated load case */
        loadCase:LoadCase[A],
        /** Results at nodes */
        nodeResults:Map[Int,NodeResult] = Map(),
        /** Results in elements */
        elementResults:Map[Int,ElementResult[A]] = Map()
) {
    
    lazy val maxDX:NodeResult = findMax(_.displacement.dx)(hasDisplacement)
    lazy val maxDY:NodeResult = findMax(_.displacement.dy)(hasDisplacement)
    lazy val maxDZ:NodeResult = findMax(_.displacement.dz)(hasDisplacement)
    lazy val maxRX:NodeResult = findMax(_.displacement.rx)(hasDisplacement)
    lazy val maxRY:NodeResult = findMax(_.displacement.ry)(hasDisplacement)
    lazy val maxRZ:NodeResult = findMax(_.displacement.rz)(hasDisplacement)
    
    lazy val minDX:NodeResult = findMin(_.displacement.dx)(hasDisplacement)
    lazy val minDY:NodeResult = findMin(_.displacement.dy)(hasDisplacement)
    lazy val minDZ:NodeResult = findMin(_.displacement.dz)(hasDisplacement)
    lazy val minRX:NodeResult = findMin(_.displacement.rx)(hasDisplacement)
    lazy val minRY:NodeResult = findMin(_.displacement.ry)(hasDisplacement)
    lazy val minRZ:NodeResult = findMin(_.displacement.rz)(hasDisplacement)
    
    lazy val maxFX:NodeResult = findMax(_.force.fx)(hasForce)
    lazy val maxFY:NodeResult = findMax(_.force.fy)(hasForce)
    lazy val maxFZ:NodeResult = findMax(_.force.fz)(hasForce)
    
    lazy val minFX:NodeResult = findMin(_.force.fx)(hasForce)
    lazy val minFY:NodeResult = findMin(_.force.fy)(hasForce)
    lazy val minFZ:NodeResult = findMin(_.force.fz)(hasForce)
    
    lazy val maxMXX:NodeResult = findMax(_.stress.get.mxx)(hasStress)
    lazy val maxMYY:NodeResult = findMax(_.stress.get.myy)(hasStress)
    lazy val maxMXY:NodeResult = findMax(_.stress.get.mxy)(hasStress)
    
    lazy val minMXX:NodeResult = findMin(_.stress.get.mxx)(hasStress)
    lazy val minMYY:NodeResult = findMin(_.stress.get.myy)(hasStress)
    lazy val minMXY:NodeResult = findMin(_.stress.get.mxy)(hasStress)
       
    private def find(compare:(Double,Double)=>Boolean)(get:(NodeResult)=>Option[Double])(filter:(NodeResult)=>Boolean) = {
      val nr = nodeResults.values.filter(filter)
      nr.tail.foldLeft(nr.head)((p,nr) => if(get(p).isDefined && get(nr).isDefined && compare(get(p).get,get(nr).get)) nr else p)
    }

    private def findMax:((NodeResult)=>Option[Double]) => ((org.encalmo.fea.NodeResult) => Boolean) => org.encalmo.fea.NodeResult = find((p,c) => c > p)
    
    private def findMin:((NodeResult)=>Option[Double]) => ((org.encalmo.fea.NodeResult) => Boolean) => org.encalmo.fea.NodeResult = find((p,c) => c < p)

    private def hasDisplacement(nr:NodeResult):Boolean = nr.hasDisplacement
    
    private def hasStress(nr:NodeResult):Boolean = nr.hasStress
    
    private def hasForce(nr:NodeResult):Boolean = nr.hasForce
    
    private def complete(nr:NodeResult):Boolean = nr.isComplete
}
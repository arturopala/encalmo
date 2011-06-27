package org.encalmo.fea

/** Mesh displacements, stresses and nodal forces */
case class LoadResults[A <: FiniteElement](
        /** Calculated load case */
        loadCase:LoadCase[A],
        /** Results at nodes */
        nodeResults:Seq[NodeResults] = Seq()
) {
    
    lazy val maxD1:NodeResults = findMax(_.D1)
    lazy val maxD2:NodeResults = findMax(_.D2)
    lazy val maxD3:NodeResults = findMax(_.D3)
    lazy val maxD4:NodeResults = findMax(_.D4)
    lazy val maxD5:NodeResults = findMax(_.D5)
    lazy val maxD6:NodeResults = findMax(_.D6)
    
    lazy val minD1:NodeResults = findMin(_.D1)
    lazy val minD2:NodeResults = findMin(_.D2)
    lazy val minD3:NodeResults = findMin(_.D3)
    lazy val minD4:NodeResults = findMin(_.D4)
    lazy val minD5:NodeResults = findMin(_.D5)
    lazy val minD6:NodeResults = findMin(_.D6)
       
    private def find(compare:(Double,Double)=>Boolean)(get:(NodeResults)=>Option[Double]) = 
        nodeResults.tail.foldLeft(nodeResults.head)((p,nr) => if(get(p).isDefined && get(nr).isDefined && compare(get(p).get,get(nr).get)) nr else p)

    private def findMax:((NodeResults)=>Option[Double])=>NodeResults = find((p,c) => c > p)
    
    private def findMin:((NodeResults)=>Option[Double])=>NodeResults = find((p,c) => c < p)

}
package org.encalmo.fea

/** Resulting displacements, stresses and nodal forces */
case class LoadResults[A <: FiniteElement](
        /** Calculated load case */
        loadCase:LoadCase[A],
        /** Results at nodes */
        nodeResults:Map[Int,NodeResult] = Map(),
        /** Results in elements */
        elementResults:Map[Int,ElementResult[A]] = Map()
) {
    
    /** Collector of selected results */
    private val collector:scala.collection.mutable.Map[Selector,Option[NodeResult]] = scala.collection.mutable.Map(Selector.selectors.map((_,None)):_*)
    
    // Processes all node's results by selectors
    nodeResults.values.foreach(process(_))
    
    /** Processes single node's result by selectors */
    private def process(nr:NodeResult):Unit = Selector.selectors.foreach(s => {collector(s) = s.select(collector(s),nr)})
    
    lazy val maxDX:NodeResult = collector(SelMaxDX).getOrElse(null)
    lazy val maxDY:NodeResult = collector(SelMaxDY).getOrElse(null)
    lazy val maxDZ:NodeResult = collector(SelMaxDZ).getOrElse(null)
    lazy val maxRX:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val maxRY:NodeResult = collector(SelMinDY).getOrElse(null)
    lazy val maxRZ:NodeResult = collector(SelMinDZ).getOrElse(null)
    
    lazy val minDX:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val minDY:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val minDZ:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val minRX:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val minRY:NodeResult = collector(SelMinDX).getOrElse(null)
    lazy val minRZ:NodeResult = collector(SelMinDX).getOrElse(null)
    
    lazy val maxFXspan:NodeResult = collector(SelMaxFXSpan).getOrElse(null)
    lazy val maxFYspan:NodeResult = collector(SelMaxFYSpan).getOrElse(null)
    lazy val maxFZspan:NodeResult = collector(SelMaxFZSpan).getOrElse(null)
    lazy val maxMXspan:NodeResult = collector(SelMaxMXSpan).getOrElse(null)
    lazy val maxMYspan:NodeResult = collector(SelMaxMYSpan).getOrElse(null)
    lazy val maxMZspan:NodeResult = collector(SelMaxMZSpan).getOrElse(null)
    
    lazy val minFXspan:NodeResult = collector(SelMinFXSpan).getOrElse(null)
    lazy val minFYspan:NodeResult = collector(SelMinFYSpan).getOrElse(null)
    lazy val minFZspan:NodeResult = collector(SelMinFZSpan).getOrElse(null)
    lazy val minMXspan:NodeResult = collector(SelMinMXSpan).getOrElse(null)
    lazy val minMYspan:NodeResult = collector(SelMinMYSpan).getOrElse(null)
    lazy val minMZspan:NodeResult = collector(SelMinMZSpan).getOrElse(null)
    
    lazy val maxFXsupp:NodeResult = collector(SelMaxFXSupport).getOrElse(null)
    lazy val maxFYsupp:NodeResult = collector(SelMaxFYSupport).getOrElse(null)
    lazy val maxFZsupp:NodeResult = collector(SelMaxFZSupport).getOrElse(null)
    lazy val maxMXsupp:NodeResult = collector(SelMaxMXSupport).getOrElse(null)
    lazy val maxMYsupp:NodeResult = collector(SelMaxMYSupport).getOrElse(null)
    lazy val maxMZsupp:NodeResult = collector(SelMaxMZSupport).getOrElse(null)
    
    lazy val minFXsupp:NodeResult = collector(SelMinFXSupport).getOrElse(null)
    lazy val minFYsupp:NodeResult = collector(SelMinFYSupport).getOrElse(null)
    lazy val minFZsupp:NodeResult = collector(SelMinFZSupport).getOrElse(null)
    lazy val minMXsupp:NodeResult = collector(SelMinMXSupport).getOrElse(null)
    lazy val minMYsupp:NodeResult = collector(SelMinMYSupport).getOrElse(null)
    lazy val minMZsupp:NodeResult = collector(SelMinMZSupport).getOrElse(null)
    
    lazy val maxSIGXX:NodeResult = collector(SelMaxSIGXX).getOrElse(null)
    lazy val maxSIGYY:NodeResult = collector(SelMaxSIGYY).getOrElse(null)
    lazy val maxSIGZZ:NodeResult = collector(SelMaxSIGZZ).getOrElse(null)
    lazy val maxTAUXY:NodeResult = collector(SelMaxTAUXY).getOrElse(null)
    lazy val maxTAUXZ:NodeResult = collector(SelMaxTAUXZ).getOrElse(null)
    lazy val maxTAUYZ:NodeResult = collector(SelMaxTAUYZ).getOrElse(null)
    lazy val maxSIGV:NodeResult = collector(SelMaxSIGV).getOrElse(null)
    lazy val maxMXXsupp:NodeResult = collector(SelMaxMXXSupport).getOrElse(null)
    lazy val maxMYYsupp:NodeResult = collector(SelMaxMYYSupport).getOrElse(null)
    lazy val maxMXXspan:NodeResult = collector(SelMaxMXXSpan).getOrElse(null)
    lazy val maxMYYspan:NodeResult = collector(SelMaxMYYSpan).getOrElse(null)
    lazy val maxMXY:NodeResult = collector(SelMaxMXY).getOrElse(null)
    lazy val maxQYZsupp:NodeResult = collector(SelMaxQYZSupport).getOrElse(null)
    lazy val maxQXZsupp:NodeResult = collector(SelMaxQXZSupport).getOrElse(null)
    lazy val maxQYZspan:NodeResult = collector(SelMaxQYZSpan).getOrElse(null)
    lazy val maxQXZspan:NodeResult = collector(SelMaxQXZSpan).getOrElse(null)
    
    lazy val minSIGXX:NodeResult = collector(SelMinSIGXX).getOrElse(null)
    lazy val minSIGYY:NodeResult = collector(SelMinSIGYY).getOrElse(null)
    lazy val minSIGZZ:NodeResult = collector(SelMinSIGZZ).getOrElse(null)
    lazy val minTAUXY:NodeResult = collector(SelMinTAUXY).getOrElse(null)
    lazy val minTAUXZ:NodeResult = collector(SelMinTAUXZ).getOrElse(null)
    lazy val minTAUYZ:NodeResult = collector(SelMinTAUYZ).getOrElse(null)
    lazy val minSIGV:NodeResult = collector(SelMinSIGV).getOrElse(null)
    lazy val minMXXsupp:NodeResult = collector(SelMinMXXSupport).getOrElse(null)
    lazy val minMYYsupp:NodeResult = collector(SelMinMYYSupport).getOrElse(null)
    lazy val minMXXspan:NodeResult = collector(SelMinMXXSpan).getOrElse(null)
    lazy val minMYYspan:NodeResult = collector(SelMinMYYSpan).getOrElse(null)
    lazy val minMXY:NodeResult = collector(SelMinMXY).getOrElse(null)
    lazy val minQYZsupp:NodeResult = collector(SelMinQYZSupport).getOrElse(null)
    lazy val minQXZsupp:NodeResult = collector(SelMinQXZSupport).getOrElse(null)
    lazy val minQYZspan:NodeResult = collector(SelMinQYZSpan).getOrElse(null)
    lazy val minQXZspan:NodeResult = collector(SelMinQXZSpan).getOrElse(null)

    /** Finds result at the node with coordinates */
    def findAt(coord:Vector):Option[NodeResult] = nodeResults.values.filter(_.node.coordinates.equals(coord)).toSeq match {case Seq(nr) => Some(nr); case  _ => None}
    /** Makes results report */
    def report:String = {
        val sb = new StringBuilder
        sb.append("Element: ")
        sb.append(loadCase.mesh.attr.name)
        sb.append(" ")
        sb.append(loadCase.mesh.attr.dimension)
        sb.append("D ")
        sb.append(loadCase.mesh.attr.dof)
        sb.append(" degree of node's freedom.")
        sb.append("\r\n")
        sb.append("Mesh: ")
        sb.append(loadCase.mesh.size)
        sb.append(" elements, ")
        sb.append(loadCase.mesh.nodes.size)
        sb.append(" nodes, ")
        sb.append(loadCase.matlines)
        sb.append(" material(s).")
        sb.append("\r\n")
        sb.append("Results:\r\n")
        Selector.selectors.foreach(s => collector(s) match {
            case Some(nr) => s.getfx(nr) match {
                case Some(d) if !Vector.equals(d,0) => {
                    sb.append("\t")
                    sb.append(s.name)
                    sb.append(" = ")
                    sb.append(DoubleFormat.short(d))
                    sb.append(" (node #")
                    sb.append(nr.node.no)
                    sb.append(" ")
                    sb.append(nr.node.positionDescription)
                    sb.append(" at ")
                    sb.append(nr.node.coordinates.explain)
                    sb.append(")\r\n")
                }
                case _ =>
            }
            case None =>
        })
        sb.toString
    }
}

// Selectors definitions

object SelMaxDX extends MaxSelector("max dx",_.hasDisplacement, _.displacement.dx)
object SelMaxDY extends MaxSelector("max dy",_.hasDisplacement, _.displacement.dy)
object SelMaxDZ extends MaxSelector("max dz",_.hasDisplacement, _.displacement.dz)
object SelMaxRX extends MaxSelector("max rx",_.hasDisplacement, _.displacement.rx)
object SelMaxRY extends MaxSelector("max ry",_.hasDisplacement, _.displacement.ry)
object SelMaxRZ extends MaxSelector("max rz",_.hasDisplacement, _.displacement.rz)

object SelMinDX extends MinSelector("min dx",_.hasDisplacement, _.displacement.dx)
object SelMinDY extends MinSelector("min dy",_.hasDisplacement, _.displacement.dy)
object SelMinDZ extends MinSelector("min dz",_.hasDisplacement, _.displacement.dz)
object SelMinRX extends MinSelector("min rx",_.hasDisplacement, _.displacement.rx)
object SelMinRY extends MinSelector("min ry",_.hasDisplacement, _.displacement.ry)
object SelMinRZ extends MinSelector("min rz",_.hasDisplacement, _.displacement.rz)

object SelMaxFXSpan extends MaxSelector("max fx (span)",_.hasDisplacementAndNotConditionForDX, _.force.fx)
object SelMaxFYSpan extends MaxSelector("max fy (span)",_.hasDisplacementAndNotConditionForDY, _.force.fy)
object SelMaxFZSpan extends MaxSelector("max fz (span)",_.hasDisplacementAndNotConditionForDZ, _.force.fz)
object SelMaxMXSpan extends MaxSelector("max mx (span)",_.hasDisplacementAndNotConditionForRX, _.force.mx)
object SelMaxMYSpan extends MaxSelector("max my (span)",_.hasDisplacementAndNotConditionForRY, _.force.my)
object SelMaxMZSpan extends MaxSelector("max mz (span)",_.hasDisplacementAndNotConditionForRZ, _.force.mz)

object SelMinFXSpan extends MinSelector("min fx (span)",_.hasDisplacementAndNotConditionForDX, _.force.fx)
object SelMinFYSpan extends MinSelector("min fy (span)",_.hasDisplacementAndNotConditionForDY, _.force.fy)
object SelMinFZSpan extends MinSelector("min fz (span)",_.hasDisplacementAndNotConditionForDZ, _.force.fz)
object SelMinMXSpan extends MinSelector("min mx (span)",_.hasDisplacementAndNotConditionForRX, _.force.mx)
object SelMinMYSpan extends MinSelector("min my (span)",_.hasDisplacementAndNotConditionForRY, _.force.my)
object SelMinMZSpan extends MinSelector("min mz (span)",_.hasDisplacementAndNotConditionForRZ, _.force.mz)

object SelMaxFXSupport extends MaxSelector("max fx (support)",_.hasDisplacementAndConditionForDX, _.force.fx)
object SelMaxFYSupport extends MaxSelector("max fy (support)",_.hasDisplacementAndConditionForDY, _.force.fy)
object SelMaxFZSupport extends MaxSelector("max fz (support)",_.hasDisplacementAndConditionForDZ, _.force.fz)
object SelMaxMXSupport extends MaxSelector("max mx (support)",_.hasDisplacementAndConditionForRX, _.force.mx)
object SelMaxMYSupport extends MaxSelector("max my (support)",_.hasDisplacementAndConditionForRY, _.force.my)
object SelMaxMZSupport extends MaxSelector("max mz (support)",_.hasDisplacementAndConditionForRZ, _.force.mz)

object SelMinFXSupport extends MinSelector("min fx (support)",_.hasDisplacementAndConditionForDX, _.force.fx)
object SelMinFYSupport extends MinSelector("min fy (support)",_.hasDisplacementAndConditionForDY, _.force.fy)
object SelMinFZSupport extends MinSelector("min fz (support)",_.hasDisplacementAndConditionForDZ, _.force.fz)
object SelMinMXSupport extends MinSelector("min mx (support)",_.hasDisplacementAndConditionForRX, _.force.mx)
object SelMinMYSupport extends MinSelector("min my (support)",_.hasDisplacementAndConditionForRY, _.force.my)
object SelMinMZSupport extends MinSelector("min mz (support)",_.hasDisplacementAndConditionForRZ, _.force.mz)

object SelMaxSIGXX extends MaxSelector("max sigxx",_.hasStress, _.stress.get.sigxx)
object SelMaxSIGYY extends MaxSelector("max sigyy",_.hasStress, _.stress.get.sigyy)
object SelMaxSIGZZ extends MaxSelector("max sigzz",_.hasStress, _.stress.get.sigzz)
object SelMaxTAUXY extends MaxSelector("max tauxy",_.hasStress, _.stress.get.tauxy)
object SelMaxTAUXZ extends MaxSelector("max tauxz",_.hasStress, _.stress.get.tauxz)
object SelMaxTAUYZ extends MaxSelector("max tauyz",_.hasStress, _.stress.get.tauyz)
object SelMaxSIGV extends MaxSelector("max sigv",_.hasStress, _.stress.get.sigv)
object SelMaxMXXSupport extends MaxSelector("max mxx (support)",_.hasStressAndConditionForRY, _.stress.get.mxx)
object SelMaxMYYSupport extends MaxSelector("max myy (support)",_.hasStressAndConditionForRX, _.stress.get.myy)
object SelMaxMXXSpan extends MaxSelector("max mxx (span)",_.hasStressAndNotConditionForRY, _.stress.get.mxx)
object SelMaxMYYSpan extends MaxSelector("max myy (span)",_.hasStressAndNotConditionForRX, _.stress.get.myy)
object SelMaxMXY extends MaxSelector("max mxy",_.hasStress, _.stress.get.mxy)
object SelMaxQYZSupport extends MaxSelector("max qyz (support)",_.hasStressAndConditionForDZ, _.stress.get.qyz)
object SelMaxQXZSupport extends MaxSelector("max qxz (support)",_.hasStressAndConditionForDZ, _.stress.get.qxz)
object SelMaxQYZSpan extends MaxSelector("max qyz (span)",_.hasStressAndNotConditionForDZ, _.stress.get.qyz)
object SelMaxQXZSpan extends MaxSelector("max qxz (span)",_.hasStressAndNotConditionForDZ, _.stress.get.qxz)

object SelMinSIGXX extends MinSelector("min sigxx",_.hasStress, _.stress.get.sigxx)
object SelMinSIGYY extends MinSelector("min sigyy",_.hasStress, _.stress.get.sigyy)
object SelMinSIGZZ extends MinSelector("min sigzz",_.hasStress, _.stress.get.sigzz)
object SelMinTAUXY extends MinSelector("min tauxy",_.hasStress, _.stress.get.tauxy)
object SelMinTAUXZ extends MinSelector("min tauxz",_.hasStress, _.stress.get.tauxz)
object SelMinTAUYZ extends MinSelector("min tauyz",_.hasStress, _.stress.get.tauyz)
object SelMinSIGV extends MinSelector("min sigv",_.hasStress, _.stress.get.sigv)
object SelMinMXXSupport extends MinSelector("min mxx (support)",_.hasStressAndConditionForRY, _.stress.get.mxx)
object SelMinMYYSupport extends MinSelector("min myy (support)",_.hasStressAndConditionForRX, _.stress.get.myy)
object SelMinMXXSpan extends MinSelector("min mxx (span)",_.hasStressAndNotConditionForRY, _.stress.get.mxx)
object SelMinMYYSpan extends MinSelector("min myy (span)",_.hasStressAndNotConditionForRX, _.stress.get.myy)
object SelMinMXY extends MinSelector("min mxy",_.hasStress, _.stress.get.mxy)
object SelMinQYZSupport extends MinSelector("min qyz (support)",_.hasStressAndConditionForDZ, _.stress.get.qyz)
object SelMinQXZSupport extends MinSelector("min qxz (support)",_.hasStressAndConditionForDZ, _.stress.get.qxz)
object SelMinQYZSpan extends MinSelector("min qyz (span)",_.hasStressAndNotConditionForDZ, _.stress.get.qyz)
object SelMinQXZSpan extends MinSelector("min qxz (span)",_.hasStressAndNotConditionForDZ, _.stress.get.qxz)

/** Selector companion object */
object Selector {
    
    /** Sequence of selectors */
    val selectors:Seq[Selector] = Seq(
            SelMaxDX,SelMaxDY,SelMaxDZ,SelMaxRX,SelMaxRY,SelMaxRZ,
            SelMinDX,SelMinDY,SelMinDZ,SelMinRX,SelMinRY,SelMinRZ,
            SelMaxFXSpan,SelMaxFYSpan,SelMaxFZSpan,SelMaxMXSpan,SelMaxMYSpan,SelMaxMZSpan,
            SelMinFXSpan,SelMinFYSpan,SelMinFZSpan,SelMinMXSpan,SelMinMYSpan,SelMinMZSpan,
            SelMaxFXSupport,SelMaxFYSupport,SelMaxFZSupport,SelMaxMXSupport,SelMaxMYSupport,SelMaxMZSupport,
            SelMinFXSupport,SelMinFYSupport,SelMinFZSupport,SelMinMXSupport,SelMinMYSupport,SelMinMZSupport,
            SelMaxSIGXX,SelMaxSIGYY,SelMaxSIGZZ,SelMaxTAUXY,SelMaxTAUXZ,SelMaxTAUYZ,SelMaxSIGV,SelMaxMXXSupport,SelMaxMYYSupport,SelMaxMXXSpan,SelMaxMYYSpan,SelMaxMXY,SelMaxQYZSupport,SelMaxQXZSupport,SelMaxQYZSpan,SelMaxQXZSpan, 
            SelMinSIGXX,SelMinSIGYY,SelMinSIGZZ,SelMinTAUXY,SelMinTAUXZ,SelMinTAUYZ,SelMinSIGV,SelMinMXXSupport,SelMinMYYSupport,SelMinMXXSpan,SelMinMYYSpan,SelMinMXY,SelMinQYZSupport,SelMinQXZSupport,SelMinQYZSpan,SelMinQXZSpan
    )
    
    /** Compare function: maximum */
    def max(d1:Double,d2:Double):Boolean = d1 > d2
    /** Compare function: minimum */
    def min(d1:Double,d2:Double):Boolean = d1 < d2
    
}

/** Selector trait */
trait Selector {
    
    /** Selector's name */
    def name:String
    /** Selects old or new result */
    def select(o:Option[NodeResult],n:NodeResult):Option[NodeResult]
    /** Value extractor function */
    def getfx:(NodeResult)=>Option[Double]
    
}

/** 3 function based Selector */
class FxSelector (
        /** Selector's name */
        val name:String,
        /** Filter */
        val filter:(NodeResult)=>Boolean,
        /** Extractor */
        val getfx:(NodeResult)=>Option[Double],
        /** Comparator */
        val compare:(Double,Double)=>Boolean
        
) extends Selector {
    
    /** Selects new value if pasess filter and compare functions */
    override def select(o:Option[NodeResult],n:NodeResult):Option[NodeResult] = {
        try {
            if(filter(n)){
                o match {
                    case Some(on) => {
                        if(getfx(n).isDefined && compare(getfx(n).get, getfx(on).get)) {
                            Some(n)
                        } else {
                            o
                        }
                    }
                    case None => Some(n)
                }
            }else{
                o
            }
        }
        catch {
            case e:Exception => o
        }
    }
}

/** Maximum value selector */
class MaxSelector(name:String, filter:(NodeResult)=>Boolean, getfx:(NodeResult)=>Option[Double]) extends FxSelector(name,filter,getfx,Selector.max)
/** Minimum value selector */
class MinSelector(name:String, filter:(NodeResult)=>Boolean, getfx:(NodeResult)=>Option[Double]) extends FxSelector(name,filter,getfx,Selector.min)

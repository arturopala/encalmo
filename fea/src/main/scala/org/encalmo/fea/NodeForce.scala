package org.encalmo.fea

/** Final element node's force components */
case class NodeForce(
        /** Force along X axis */
        fx:Option[Double] = None,
        /** Force along Y axis */
        fy:Option[Double] = None,
        /** Force along Z axis */
        fz:Option[Double] = None,
        /** Bending moment around X axis */
        mx:Option[Double] = None,
        /** Bending moment around Y axis */
        my:Option[Double] = None,
        /** Bending moment around Z axis */
        mz:Option[Double] = None
){
    /** Force component's sequence */
    lazy val seq = Seq(fx,fy,fz,mx,my,mz)
    /** Returns true if any of components is defined */
    def isDefined:Boolean = seq.exists(_.isDefined)
    /** Force component's sequence paired with labels*/
    def explainseq = Seq(("fx",fx),("fy",fy),("fz",fz),("mx",mx),("my",my),("mz",mz))
    /** Verbose explanation of force */
    def explain:String = explainseq.foldLeft[String]("")((s,p) => {if(p._2.isDefined) s + p._1 + "=" + DoubleFormat.short(p._2.get) + " " else s})
}

/** Node force factory */
object NodeForce {
    
    /** Plate node displacement: dz,rx,ry */
    def apply(fx:Double,fy:Double,fz:Double):NodeForce = NodeForce(Some(fx),Some(fy),Some(fz))
    def apply(seq:Seq[Option[Double]]):NodeForce = NodeForce(seq(0),seq(1),seq(2))
    def apply(optseq:Option[Seq[Option[Double]]]):NodeForce = optseq.map(seq => NodeForce(seq(0),seq(1),seq(2))).getOrElse(NodeForce())
    
}
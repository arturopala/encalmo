package org.encalmo.fea

/** Final element node's displacement components */
case class NodeDisplacement(
        /** Translation along X axis */
        dx:Option[Double] = None,
        /** Translation along Y axis */
        dy:Option[Double] = None,
        /** Translation along Z axis */
        dz:Option[Double] = None,
        /** Rotation around X axis */
        rx:Option[Double] = None,
        /** Rotation around Y axis */
        ry:Option[Double] = None,
        /** Rotation around Z axis */
        rz:Option[Double] = None
){
    /** Displacement component's sequence */
    lazy val seq = Seq(dx,dy,dz,rx,ry,rz)
    /** Returns true if any of components is defined */
    def isDefined:Boolean = seq.exists(_.isDefined)
    /** Displacement component's sequence paired with labels*/
    def explainseq = Seq(("dx",dx),("dy",dy),("dz",dz),("rx",rx),("ry",ry),("rz",rz))
    /** Verbose explanation of displacement */
    def explain:String = explainseq.foldLeft[String]("")((s,p) => {if(p._2.isDefined) s + p._1 + "=" + DoubleFormat.short(p._2.get) + " " else s})
    /** Returns true if this displacement meets argument's displacement conditions */
    def meets(nd:NodeDisplacement) = nd.seq.zip(seq).forall(_ match {
        case (None,None) => true
        case (None,Some(d)) => true
        case (Some(d),None) => true
        case (Some(d1),Some(d2)) => Vector.equals(d1,d2)
    })
}
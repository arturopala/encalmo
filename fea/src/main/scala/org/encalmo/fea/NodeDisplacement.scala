package org.encalmo.fea

/** Final element node's displacement */
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
    def explain:String = explainseq.foldLeft[String]("")((s,p) => {if(p._2.isDefined) (s + p._1+"="+DoubleFormat.short(p._2.get)+" ") else s})
}

/** Node displacement factory */
object NodeDisplacement {
    
    /** Plate node displacement: dz,rx,ry */
    def forPlate(dz:Double,rx:Double,ry:Double) = NodeDisplacement(None,None,Some(dz),Some(rx),Some(ry),None)
    def forPlate(dz:Option[Double] = None,rx:Option[Double] = None, ry:Option[Double] = None) = NodeDisplacement(None,None,dz,rx,ry,None)
    def forPlate(seq:Seq[Option[Double]]) = NodeDisplacement(None,None,seq(0),seq(1),seq(2),None)
    
}
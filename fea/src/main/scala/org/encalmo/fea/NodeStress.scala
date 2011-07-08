package org.encalmo.fea

/** Final element node stress */
case class NodeStress(
        /** Normal stress along X axis */
        sigxx:Option[Double] = None,
        /** Normal stress along Y axis */
        sigyy:Option[Double] = None,
        /** Normal stress along Z axis */
        sigzz:Option[Double] = None,
        /** Shear stress on xy plane */
        tauxy:Option[Double] = None,
        /** Shear stress on xz plane */
        tauxz:Option[Double] = None,
        /** Shear stress on yz plane */
        tauyz:Option[Double] = None,
        /** Von Misses reduced stress */
        sigv:Option[Double] = None,
        /** Bending moment around X axis */
        mxx:Option[Double] = None,
        /** Bending moment around Y axis */
        myy:Option[Double] = None,
        /** Torsion moment around Z axis */
        mxy:Option[Double] = None,
        /** Shear force QYZ */
        qyz:Option[Double] = None,
        /** Shear force QZX */
        qzx:Option[Double] = None
){
    /** Force component's sequence */
    lazy val seq = Seq(sigxx,sigyy,sigzz,tauxy,tauxz,tauyz,sigv,mxx,myy,qyz,qzx)
    /** Returns true if any of components is defined */
    def isDefined:Boolean = seq.exists(_.isDefined)
    /** Force component's sequence paired with labels*/
    def explainseq = Seq("sigxx","sigyy","sigzz","tauxy","tauxz","tauyz","sigv","mxx","myy","qyz","qzx") zip seq
    /** Verbose explanation of force */
    def explain:String = explainseq.foldLeft[String]("")((s,p) => {if(p._2.isDefined) (s + p._1+"="+DoubleFormat.short(p._2.get)+" ") else s})
    /** Aggregates two NodeStress into one */
    def aggregate(o:NodeStress):NodeStress = NodeStress(seq.zip(o.seq).map(p => worse(p)))
    /** Takes worse of two values */
    private def worse(p:(Option[Double], Option[Double])) = p match {
      case (None,None) => None
      case (None,Some(d)) => p._2
      case (Some(d),None) => p._1
      case (v1,v2) => Math.abs(v1.get)-Math.abs(v2.get) match {
        	case x if x<0 => v2
        	case _ => v1
      }
    }
    
}

/** Node stress factory */
object NodeStress {
  
	def apply(s:Seq[Option[Double]]):NodeStress = NodeStress(s(0),s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10))
  
	/** Aggregates */
	def aggregate(seq:Seq[NodeStress]):NodeStress = seq.tail.foldLeft[NodeStress](seq.head)((l,r) => l aggregate r)
    
    /** Plate corner stresses: MXX MYY MXY QYZ QZX SIGXX SIGYY TAUXY TAUXZ(Z=0) TAUYZ(Z=0) */
    def forPlateElementCorners(seq:Seq[Option[Double]]):NodeStress = NodeStress(seq(5),seq(6),None,seq(7),seq(8),seq(9),None,seq(0),seq(1),seq(2),seq(3),seq(4))
    /** Plate gauss points stresses: MXX MYY MXY QYZ QZX SIGXX SIGYY TAUXY TAUXZ(Z=0) TAUYZ(Z=0) SIGV */
    def forPlateGaussPoints(seq:Seq[Option[Double]]):NodeStress = NodeStress(seq(5),seq(6),None,seq(7),seq(8),seq(9),seq(10),seq(0),seq(1),seq(2),seq(3),seq(4))
    
}
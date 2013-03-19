package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.structures.common.section._

import SectionSymbols._
import IBeamSectionSymbols._

/**
* European standard beams
* Flange slope: 14%
* Dimensions: DIN 1025-1: 1995, NF A 45-209 (1983)
* Tolerances: EN 10024: 1995
* Surface condition according to EN 10163-3: 2004, class C, subclass 1
*/
class IPNSection(id:String) extends IBeamSection(id,"IPNSection") {

	override def descriptionRef:String = "IPNSection_description"
}

/**
* IPNSection factory
*/
object IPNSection {

	def apply(
		/** Section ID */p_id:String,
		/** Mass */p_m:Double,
		/** Beam height */p_h:Double,
		/** Flange width */p_b:Double,
		/** Web thickness */p_tw:Double,
		/** Flange thickness */p_tf:Double,
		/** Radius */p_r:Double,
		/** Radius */p_r2:Double,
		/** Section area */p_A:Double,
		/** Web length */p_hd:Double,
		/** Diameter */p_phi:Double,
		/** */p_pmin:Double,
		/** */p_pmax:Double,
		/** Surface per unit length */p_AL:Double,
		/** Surface per unit mass */p_AG:Double,
		/** */p_Iy:Double,
		/** */p_Wyel:Double,
		/** */p_Wypl:Double,
		/** */p_iy:Double,
		/** */p_AVz:Double,
		/** */p_Iz:Double,
		/** */p_Wzel:Double,
		/** */p_Wzpl:Double,
		/** */p_iz:Double,
		/** */p_ss:Double,
		/** */p_It:Double,
		/** */p_Iw:Double,
		/** */p_f1:Double,
		/** */p_f2:Double,
		/** */p_f3:Double,
		/** */p_f4:Double
	):IPNSection = {
		new IPNSection(p_id){
			this(h)=Number(p_h*1E-3)
			this(b)=Number(p_b*1E-3)
			this(tw)=Number(p_tw*1E-3)
			this(tf)=Number(p_tf*1E-3)
			this(hd)=Number(p_hd*1E-3)
			this(ss)=Number(p_ss*1E-3)
			this(phi)=Number(p_phi)
			this(pmin)=Number(p_pmin*1E-3)
			this(pmax)=Number(p_pmax*1E-3)
			this(r)=Number(p_r*1E-3)
			this(r2)=Number(p_r2*1E-3)
			this(A)=Number(p_A*1E-4)
			this(Iy)=Number(p_Iy*1E-8)
			this(Iz)=Number(p_Iz*1E-8)
			this(Wy)=Number(p_Wyel*1E-6)
			this(Wz)=Number(p_Wzel*1E-6)
			this(iy)=Number(p_iy*1E-2)
			this(iz)=Number(p_iz*1E-2)
			this(m)=Number(p_m)
			this(AL)=Number(p_AL)
			this(AG)=Number(p_AG)
			this(AVz)=Number(p_AVz*1E-4)
			this(Wypl)=Number(p_Wypl*1E-6)
			this(Wzpl)=Number(p_Wzpl*1E-6)
			this(It)=Number(p_It*1E-8)
			this(Iomega)=Number(p_Iw*1E-9)
			this(f1)=Number(p_f1)
			this(f2)=Number(p_f2)
			this(f3)=Number(p_f3)
			this(f4)=Number(p_f4)
			lock
		}
	}

	/*---------------------------------------
	 *	IPNSection
	 *---------------------------------------*/

	def apply(s:String):IPNSection = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)

	val map = Map[String,()=>IPNSection](
		"IPN 80" -> IPN_80 _,
		"IPN 100" -> IPN_100 _,
		"IPN 120" -> IPN_120 _,
		"IPN 140" -> IPN_140 _,
		"IPN 160" -> IPN_160 _,
		"IPN 180" -> IPN_180 _,
		"IPN 200" -> IPN_200 _,
		"IPN 220" -> IPN_220 _,
		"IPN 240" -> IPN_240 _,
		"IPN 260" -> IPN_260 _,
		"IPN 280" -> IPN_280 _,
		"IPN 300" -> IPN_300 _,
		"IPN 320" -> IPN_320 _,
		"IPN 340" -> IPN_340 _,
		"IPN 360" -> IPN_360 _,
		"IPN 380" -> IPN_380 _,
		"IPN 400" -> IPN_400 _,
		"IPN 450" -> IPN_450 _,
		"IPN 500" -> IPN_500 _,
		"IPN 550" -> IPN_550 _,
		"IPN 600" -> IPN_600 _
	)

	lazy val IPN_80 = IPNSection("IPN 80",5.9,80,42,3.9,5.9,3.9,2.3,7.58,59,0,0,0,0.304,51.09,77.8,19.5,22.8,3.2,3.41,6.29,3,5,0.91,21.6,0.87,0.09,346,401,266,322)
	lazy val IPN_100 = IPNSection("IPN 100",8.3,100,50,4.5,6.8,4.5,2.7,10.6,75.7,0,0,0,0.37,44.47,171,34.2,39.8,4.01,4.85,12.2,4.88,8.1,1.07,25,1.6,0.27,302,349,236,283)
	lazy val IPN_120 = IPNSection("IPN 120",11.1,120,58,5.1,7.7,5.1,3.1,14.2,92.4,0,0,0,0.439,39.38,328,54.7,63.6,4.81,6.63,21.5,7.41,12.4,1.23,28.4,2.71,0.69,268,309,210,251)
	lazy val IPN_140 = IPNSection("IPN 140",14.3,140,66,5.7,8.6,5.7,3.4,18.3,109.1,0,0,0,0.502,34.94,573,81.9,95.4,5.61,8.65,35.2,10.7,17.9,1.4,31.8,4.32,1.54,238,274,189,225)
	lazy val IPN_160 = IPNSection("IPN 160",17.9,160,74,6.3,9.5,6.3,3.8,22.8,125.8,0,0,0,0.575,32.13,935,117,136,6.4,10.83,54.7,14.8,24.9,1.55,35.2,6.57,3.14,220,252,173,205)
	lazy val IPN_180 = IPNSection("IPN 180",21.9,180,82,6.9,10.4,6.9,4.1,27.9,142.4,0,0,0,0.64,29.22,1450,161,187,7.2,13.35,81.3,19.8,33.2,1.71,38.6,9.58,5.92,200,229,158,188)
	lazy val IPN_200 = IPNSection("IPN 200",26.2,200,90,7.5,11.3,7.5,4.5,33.4,159.1,0,0,0,0.709,27.04,2140,214,250,8,16.03,117,26,43.5,1.87,42,13.5,10.5,185,212,147,174)
	lazy val IPN_220 = IPNSection("IPN 220",31.1,220,98,8.1,12.2,8.1,4.9,39.5,175.8,10,50,56,0.775,24.99,3060,278,324,8.8,19.06,162,33.1,55.7,2.02,45.4,18.6,17.8,171,196,136,161)
	lazy val IPN_240 = IPNSection("IPN 240",36.2,240,106,8.7,13.1,8.7,5.2,46.1,192.5,10,54,60,0.844,23.32,4250,354,412,9.59,22.33,221,41.7,70,2.2,48.9,25,28.7,160,183,127,150)
	lazy val IPN_260 = IPNSection("IPN 260",41.9,260,113,9.4,14.1,9.4,5.6,53.3,208.9,12,62,62,0.906,21.65,5740,442,514,10.4,26.08,288,51,85.9,2.32,52.6,33.5,44.1,149,170,119,140)
	lazy val IPN_280 = IPNSection("IPN 280",47.9,280,119,10.1,15.2,10.1,6.1,61,225.1,12,68,68,0.966,20.17,7590,542,632,11.1,30.18,364,61.2,103,2.45,56.4,44.2,64.6,139,158,111,131)
	lazy val IPN_300 = IPNSection("IPN 300",54.2,300,125,10.8,16.2,10.8,6.5,69,241.6,12,70,74,1.03,19.02,9800,653,762,11.9,34.58,451,72.2,121,2.56,60.1,56.8,91.8,131,149,105,123)
	lazy val IPN_320 = IPNSection("IPN 320",61,320,131,11.5,17.3,11.5,6.9,77.7,257.9,12,70,80,1.09,17.87,12510,782,914,12.7,39.26,555,84.7,143,2.67,63.9,72.5,129,123,140,99,116)
	lazy val IPN_340 = IPNSection("IPN 340",68,340,137,12.2,18.3,12.2,7.3,86.7,274.3,12,78,86,1.15,16.9,15700,923,1080,13.5,44.27,674,98.4,166,2.8,67.6,90.4,176,117,133,94,110)
	lazy val IPN_360 = IPNSection("IPN 360",76.1,360,143,13,19.5,13,7.8,97,290.2,12,78,92,1.21,15.89,19610,1090,1276,14.2,49.95,818,114,194,2.9,71.8,115,240,110,125,89,104)
	lazy val IPN_380 = IPNSection("IPN 380",84,380,149,13.7,20.5,13.7,8.2,107,306.7,16,84,86,1.27,15.12,24010,1260,1482,15,55.55,975,131,221,3.02,75.4,141,319,105,119,85,99)
	lazy val IPN_400 = IPNSection("IPN 400",92.4,400,155,14.4,21.6,14.4,8.6,118,322.9,16,86,92,1.33,14.36,29210,1460,1714,15.7,61.69,1160,149,253,3.13,79.3,170,420,100,113,81,94)
	lazy val IPN_450 = IPNSection("IPN 450",115,450,170,16.2,24.3,16.2,9.7,147,363.6,16,92,106,1.48,12.83,45850,2040,2400,17.7,77.79,1730,203,345,3.43,88.9,267,791,89,101,73,84)
	lazy val IPN_500 = IPNSection("IPN 500",141,500,185,18,27,18,10.8,179,404.3,20,102,110,1.63,11.6,68740,2750,3240,19.6,95.6,2480,268,456,3.72,98.5,402,1400,81,91,66,77)
	lazy val IPN_550 = IPNSection("IPN 550",166,550,200,19,30,19,11.9,212,445.6,22,112,118,1.8,10.8,99180,3610,4240,21.6,111.3,3490,349,592,4.02,107.3,544,2390,75,85,61,71)
	lazy val IPN_600 = IPNSection("IPN 600",199,600,215,21.6,32.4,21.6,13,254,485.8,24,126,128,1.97,9.89,138800,4627,5452,23.39,138,4674,435,752,4.29,117.6,787,3814,68,76,56,64)

	
	
}

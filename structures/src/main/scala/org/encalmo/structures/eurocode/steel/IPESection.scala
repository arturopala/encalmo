package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.structures.common.section._

import SectionSymbols._
import IBeamSectionSymbols._

/**
* European I beams
* Dimensions: IPE80 - 600 in accordance with Euronorm 19-57; IPE A 80 - 600; IPE O 180 - 600; IPE 750
* Tolerances: EN 10034: 1993
* Surface condition according to EN 10163-3: 2004, class C, subclass 1
*/
class IPESection(id:String) extends IBeamSection(id,"IPESection") {

	override def descriptionRef:String = "IPESection_description"
}

/**
* IPESection factory
*/
object IPESection {

	def apply(
		/** Section ID */p_id:String,
		/** Mass */p_m:Double,
		/** Beam height */p_h:Double,
		/** Flange width */p_b:Double,
		/** Web thickness */p_tw:Double,
		/** Flange thickness */p_tf:Double,
		/** Radius */p_r:Double,
		/** Section area */p_A:Double,
		/** Web height */p_hw:Double,
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
	):IPESection = {
		new IPESection(p_id){
			this(h)=Number(p_h)
			this(b)=Number(p_b)
			this(tw)=Number(p_tw)
			this(tf)=Number(p_tf)
			this(hw)=Number(p_hw)
			this(hd)=Number(p_hd)
			this(ss)=Number(p_ss)
			this(phi)=Number(p_phi)
			this(pmin)=Number(p_pmin)
			this(pmax)=Number(p_pmax)
			this(r)=Number(p_r)
			this(A)=Number(p_A)
			this(Iy)=Number(p_Iy)
			this(Iz)=Number(p_Iz)
			this(Wy)=Number(p_Wyel)
			this(Wz)=Number(p_Wzel)
			this(iy)=Number(p_iy)
			this(iz)=Number(p_iz)
			this(m)=Number(p_m)
			this(AL)=Number(p_AL)
			this(AG)=Number(p_AG)
			this(AVz)=Number(p_AVz)
			this(Wypl)=Number(p_Wypl)
			this(Wzpl)=Number(p_Wzpl)
			this(It)=Number(p_It)
			this(Iomega)=Number(p_Iw)
			this(f1)=Number(p_f1)
			this(f2)=Number(p_f2)
			this(f3)=Number(p_f3)
			this(f4)=Number(p_f4)
			lock
		}
	}

	/*---------------------------------------
	 *	IPESection
	 *---------------------------------------*/

	def apply(s:String):IPESection = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)

	val map = Map[String,()=>IPESection](
		"IPE 80 A" -> IPE_80_A _,
		"IPE 80" -> IPE_80 _,
		"IPE A 100" -> IPE_A_100 _,
		"IPE 100" -> IPE_100 _,
		"IPE A 120" -> IPE_A_120 _,
		"IPE 120" -> IPE_120 _,
		"IPE A 140" -> IPE_A_140 _,
		"IPE 140" -> IPE_140 _,
		"IPE A 160" -> IPE_A_160 _,
		"IPE 160" -> IPE_160 _,
		"IPE A 180" -> IPE_A_180 _,
		"IPE 180" -> IPE_180 _,
		"IPE O 180" -> IPE_O_180 _,
		"IPE A 200" -> IPE_A_200 _,
		"IPE 200" -> IPE_200 _,
		"IPE O 200" -> IPE_O_200 _,
		"IPE A 220" -> IPE_A_220 _,
		"IPE 220" -> IPE_220 _,
		"IPE O 220" -> IPE_O_220 _,
		"IPE A 240" -> IPE_A_240 _,
		"IPE 240" -> IPE_240 _,
		"IPE O 240" -> IPE_O_240 _,
		"IPE A 270" -> IPE_A_270 _,
		"IPE 270" -> IPE_270 _,
		"IPE O 270" -> IPE_O_270 _,
		"IPE A 300" -> IPE_A_300 _,
		"IPE 300" -> IPE_300 _,
		"IPE O 300" -> IPE_O_300 _,
		"IPE A 330" -> IPE_A_330 _,
		"IPE 330" -> IPE_330 _,
		"IPE O 330" -> IPE_O_330 _,
		"IPE A 360" -> IPE_A_360 _,
		"IPE 360" -> IPE_360 _,
		"IPE O 360" -> IPE_O_360 _,
		"IPE A 400" -> IPE_A_400 _,
		"IPE 400" -> IPE_400 _,
		"IPE O 400" -> IPE_O_400 _,
		"IPE A 450" -> IPE_A_450 _,
		"IPE 450" -> IPE_450 _,
		"IPE O 450" -> IPE_O_450 _,
		"IPE A 500" -> IPE_A_500 _,
		"IPE 500" -> IPE_500 _,
		"IPE O 500" -> IPE_O_500 _,
		"IPE A 550" -> IPE_A_550 _,
		"IPE 550" -> IPE_550 _,
		"IPE O 550" -> IPE_O_550 _,
		"IPE A 600" -> IPE_A_600 _,
		"IPE 600" -> IPE_600 _,
		"IPE O 600" -> IPE_O_600 _,
		"IPE 750 x 137" -> IPE_750_x_137 _,
		"IPE 750 x 147" -> IPE_750_x_147 _,
		"IPE 750 x 173" -> IPE_750_x_173 _,
		"IPE 750 x 196" -> IPE_750_x_196 _
	)

	lazy val IPE_80_A = IPESection("IPE 80 A",5.00,78.00,46.00,3.30,4.20,5.00,6.38,69.60,59.60,0,0,0,0.33,64.90,64.38,16.51,18.98,3.18,3.07,6.85,2.98,4.69,1.04,17.60,0.42,0.09,437,509,317,389)
	lazy val IPE_80 = IPESection("IPE 80",6.00,80.00,46.00,3.80,5.20,5.00,7.64,69.60,59.60,0,0,0,0.33,54.64,80.14,20.03,23.22,3.24,3.58,8.49,3.69,5.82,1.05,20.10,0.70,0.12,369,429,270,330)
	lazy val IPE_A_100 = IPESection("IPE A 100",6.90,98.00,55.00,3.60,4.70,7.00,8.78,88.60,74.60,0,0,0,0.40,57.57,141.20,28.81,32.98,4.01,4.44,13.12,4.77,7.54,1.22,21.20,0.77,0.28,389,452,286,349)
	lazy val IPE_100 = IPESection("IPE 100",8.10,100.00,55.00,4.10,5.70,7.00,10.30,88.60,74.60,0,0,0,0.40,49.33,171.00,34.20,39.41,4.07,5.08,15.92,5.79,9.15,1.24,23.70,1.20,0.35,334,387,247,300)
	lazy val IPE_A_120 = IPESection("IPE A 120",8.70,117.60,64.00,3.80,5.10,7.00,11.00,107.40,93.40,0,0,0,0.47,54.47,257.40,43.77,49.87,4.83,5.41,22.39,7.00,10.98,1.42,22.20,1.04,0.71,370,428,271,329)
	lazy val IPE_120 = IPESection("IPE 120",10.40,120.00,64.00,4.40,6.30,7.00,13.20,107.40,93.40,0,0,0,0.48,45.82,317.80,52.96,60.73,4.90,6.31,27.67,8.65,13.58,1.45,25.20,1.74,0.89,311,360,230,279)
	lazy val IPE_A_140 = IPESection("IPE A 140",10.50,137.40,73.00,3.80,5.60,7.00,13.40,126.20,112.20,0,0,0,0.55,52.05,434.90,63.30,71.60,5.70,6.21,36.42,9.98,15.52,1.65,23.20,1.36,1.58,354,409,260,314)
	lazy val IPE_140 = IPESection("IPE 140",12.90,140.00,73.00,4.70,6.90,7.00,16.40,126.20,112.20,0,0,0,0.55,42.70,541.20,77.32,88.34,5.74,7.64,44.92,12.31,19.25,1.65,26.70,2.45,1.98,291,335,215,259)
	lazy val IPE_A_160 = IPESection("IPE A 160",12.70,157.00,82.00,4.00,5.90,9.00,16.20,145.20,127.20,0,0,0,0.62,48.70,689.30,87.81,99.09,6.53,7.80,54.43,13.27,20.70,1.83,26.34,1.96,3.09,332,382,245,295)
	lazy val IPE_160 = IPESection("IPE 160",15.80,160.00,82.00,5.00,7.40,9.00,20.10,145.20,127.20,0,0,0,0.62,39.47,869.30,108.70,123.90,6.58,9.66,68.31,16.66,26.10,1.84,30.34,3.60,3.96,269,310,200,241)
	lazy val IPE_A_180 = IPESection("IPE A 180",15.40,177.00,91.00,4.30,6.50,9.00,19.60,164.00,146.00,10,48.00,48.00,0.69,45.15,1063.00,120.10,135.30,7.37,9.20,81.89,18.00,27.96,2.05,27.84,2.70,5.93,308,354,227,274)
	lazy val IPE_180 = IPESection("IPE 180",18.80,180.00,91.00,5.30,8.00,9.00,23.90,164.00,146.00,10,48.00,48.00,0.70,37.13,1317.00,146.30,166.40,7.42,11.25,100.90,22.16,34.60,2.05,31.84,4.79,7.43,253,291,188,226)
	lazy val IPE_O_180 = IPESection("IPE O 180",21.30,182.00,92.00,6.00,9.00,9.00,27.10,164.00,146.00,10,50.00,50.00,0.71,33.12,1505.00,165.40,189.10,7.45,12.70,117.30,25.50,39.91,2.08,34.54,6.76,8.74,226,260,168,202)
	lazy val IPE_A_200 = IPESection("IPE A 200",18.40,197.00,100.00,4.50,7.00,12.00,23.50,183.00,159.00,10,54.00,58.00,0.76,41.49,1591.00,161.60,181.70,8.23,11.47,117.20,23.43,36.54,2.23,32.56,4.11,10.53,283,326,210,253)
	lazy val IPE_200 = IPESection("IPE 200",22.40,200.00,100.00,5.60,8.50,12.00,28.50,183.00,159.00,10,54.00,58.00,0.77,34.36,1943.00,194.30,220.60,8.26,14.00,142.40,28.47,44.61,2.24,36.66,6.98,12.99,235,270,176,211)
	lazy val IPE_O_200 = IPESection("IPE O 200",25.10,202.00,102.00,6.20,9.50,12.00,32.00,183.00,159.00,10,56.00,60.00,0.78,31.05,2211.00,218.90,249.40,8.32,15.45,168.90,33.11,51.89,2.30,39.26,9.45,15.57,212,244,158,190)
	lazy val IPE_A_220 = IPESection("IPE A 220",22.20,217.00,110.00,5.00,7.70,12.00,28.30,201.60,177.60,12,60.00,62.00,0.84,38.02,2317.00,213.50,240.20,9.05,13.55,171.40,31.17,48.49,2.46,34.46,5.69,18.71,260,298,193,231)
	lazy val IPE_220 = IPESection("IPE 220",26.20,220.00,110.00,5.90,9.20,12.00,33.40,201.60,177.60,12,60.00,62.00,0.85,32.36,2772.00,252.00,285.40,9.11,15.88,204.90,37.25,58.11,2.48,38.36,9.07,22.67,221,254,165,198)
	lazy val IPE_O_220 = IPESection("IPE O 220",29.40,222.00,112.00,6.60,10.20,12.00,37.40,201.60,177.60,10,58.00,66.00,0.86,29.24,3134.00,282.30,321.10,9.16,17.66,239.80,42.83,66.91,2.53,41.06,12.27,26.79,200,230,149,179)
	lazy val IPE_A_240 = IPESection("IPE A 240",26.20,237.00,120.00,5.20,8.30,15.00,33.30,220.40,190.40,12,64.00,68.00,0.92,35.10,3290.00,277.70,311.60,9.94,16.31,240.10,40.02,62.40,2.68,39.37,8.35,31.26,240,276,178,214)
	lazy val IPE_240 = IPESection("IPE 240",30.70,240.00,120.00,6.20,9.80,15.00,39.10,220.40,190.40,12,66.00,68.00,0.92,30.02,3892.00,324.30,366.60,9.97,19.14,283.60,47.27,73.92,2.69,43.37,12.88,37.39,205,236,153,184)
	lazy val IPE_O_240 = IPESection("IPE O 240",34.30,242.00,122.00,7.00,10.80,15.00,43.70,220.40,190.40,12,66.00,70.00,0.93,27.17,4369.00,361.10,410.30,10.00,21.36,328.50,53.86,84.40,2.74,46.17,17.18,43.68,185,213,139,167)
	lazy val IPE_A_270 = IPESection("IPE A 270",30.70,267.00,135.00,5.50,8.70,15.00,39.20,249.60,219.60,16,70.00,72.00,1.04,33.75,4917.00,368.30,412.50,11.21,18.75,358.00,53.03,82.34,3.02,40.47,10.30,59.51,230,265,171,205)
	lazy val IPE_270 = IPESection("IPE 270",36.10,270.00,135.00,6.60,10.20,15.00,45.90,249.60,219.60,16,72.00,72.00,1.04,28.86,5790.00,428.90,484.00,11.23,22.14,419.90,62.20,96.95,3.02,44.57,15.94,70.58,197,227,147,176)
	lazy val IPE_O_270 = IPESection("IPE O 270",42.30,274.00,136.00,7.50,12.20,15.00,53.80,249.60,219.60,16,72.00,72.00,1.05,24.88,6947.00,507.10,574.60,11.36,25.23,513.50,75.51,117.70,3.09,49.47,24.90,87.64,170,195,127,152)
	lazy val IPE_A_300 = IPESection("IPE A 300",36.50,297.00,150.00,6.10,9.20,15.00,46.50,278.60,248.60,16,72.00,86.00,1.16,31.65,7173.00,483.10,541.80,12.42,22.25,519.00,69.20,107.30,3.34,42.07,13.43,107.20,216,248,160,192)
	lazy val IPE_300 = IPESection("IPE 300",42.20,300.00,150.00,7.10,10.70,15.00,53.80,278.60,248.60,16,72.00,86.00,1.16,27.46,8356.00,557.10,628.40,12.46,25.68,603.80,80.50,125.20,3.35,46.07,20.12,125.90,188,216,139,167)
	lazy val IPE_O_300 = IPESection("IPE O 300",49.30,304.00,152.00,8.00,12.70,15.00,62.80,278.60,248.60,16,74.00,88.00,1.17,23.81,9994.00,657.50,743.80,12.61,29.05,745.70,98.12,152.60,3.45,50.97,31.06,157.70,163,187,121,145)
	lazy val IPE_A_330 = IPESection("IPE A 330",43.00,327.00,160.00,6.50,10.00,18.00,54.70,307.00,271.00,16,78.00,96.00,1.25,29.09,10230.00,625.70,701.90,13.67,26.99,685.20,85.64,133.30,3.54,47.59,19.57,171.50,199,228,149,178)
	lazy val IPE_330 = IPESection("IPE 330",49.10,330.00,160.00,7.50,11.50,18.00,62.60,307.00,271.00,16,78.00,96.00,1.25,25.52,11770.00,713.10,804.30,13.71,30.81,788.10,98.52,153.70,3.55,51.59,28.15,199.10,175,200,131,157)
	lazy val IPE_O_330 = IPESection("IPE O 330",57.00,334.00,162.00,8.50,13.50,18.00,72.60,307.00,271.00,16,80.00,98.00,1.27,22.24,13910.00,833.00,942.80,13.84,34.88,960.40,118.60,185.00,3.64,56.59,42.15,245.70,152,175,114,137)
	lazy val IPE_A_360 = IPESection("IPE A 360",50.20,357.60,170.00,6.60,11.50,18.00,64.00,334.60,298.60,22,86.00,88.00,1.35,26.91,14520.00,811.80,906.80,15.06,29.76,944.30,111.10,171.90,3.84,50.69,26.51,282.00,185,211,138,165)
	lazy val IPE_360 = IPESection("IPE 360",57.10,360.00,170.00,8.00,12.70,18.00,72.70,334.60,298.60,22,88.00,88.00,1.35,23.70,16270.00,903.60,1019.00,14.95,35.14,1043.00,122.80,191.10,3.79,54.49,37.32,313.60,163,186,122,146)
	lazy val IPE_O_360 = IPESection("IPE O 360",66.00,364.00,172.00,9.20,14.70,18.00,84.10,334.60,298.60,22,90.00,90.00,1.37,20.69,19050.00,1047.00,1186.00,15.05,40.21,1251.00,145.50,226.90,3.86,59.69,55.76,380.30,142,162,107,127)
	lazy val IPE_A_400 = IPESection("IPE A 400",57.40,397.00,180.00,7.00,12.00,21.00,73.10,373.00,331.00,22,94.00,98.00,1.46,25.51,20290.00,1022.00,1144.00,16.66,35.78,1171.00,130.10,202.10,4.00,55.60,34.79,432.20,176,200,133,158)
	lazy val IPE_400 = IPESection("IPE 400",66.30,400.00,180.00,8.60,13.50,21.00,84.50,373.00,331.00,22,96.00,98.00,1.47,22.12,23130.00,1156.00,1307.00,16.55,42.69,1318.00,146.40,229.00,3.95,60.20,51.08,490.00,152,174,116,137)
	lazy val IPE_O_400 = IPESection("IPE O 400",75.70,404.00,182.00,9.70,15.50,21.00,96.40,373.00,331.00,22,96.00,100.00,1.48,19.57,26750.00,1324.00,1502.00,16.66,47.98,1564.00,171.90,269.10,4.03,65.30,73.10,587.60,135,154,103,122)
	lazy val IPE_A_450 = IPESection("IPE A 450",67.20,447.00,190.00,7.60,13.10,21.00,85.60,420.80,378.80,24,100.00,102.00,1.60,23.87,29760.00,1331.00,1494.00,18.65,42.26,1502.00,158.10,245.70,4.19,58.40,45.67,704.90,165,187,127,149)
	lazy val IPE_450 = IPESection("IPE 450",77.60,450.00,190.00,9.40,14.60,21.00,98.80,420.80,378.80,24,100.00,102.00,1.61,20.69,33740.00,1500.00,1702.00,18.48,50.85,1676.00,176.40,276.40,4.12,63.20,66.87,791.00,143,162,110,130)
	lazy val IPE_O_450 = IPESection("IPE O 450",92.40,456.00,192.00,11.00,17.60,21.00,118.00,420.80,378.80,24,102.00,104.00,1.62,17.56,40920.00,1795.00,2046.00,18.65,59.40,2085.00,217.20,341.00,4.21,70.80,109.00,997.60,122,138,94,110)
	lazy val IPE_A_500 = IPESection("IPE A 500",79.40,497.00,200.00,8.40,14.50,21.00,101.00,468.00,426.00,24,100.00,112.00,1.74,21.94,42930.00,1728.00,1946.00,20.61,50.41,1939.00,193.90,301.60,4.38,62.00,62.78,1125.00,152,172,118,138)
	lazy val IPE_500 = IPESection("IPE 500",90.70,500.00,200.00,10.20,16.00,21.00,116.00,468.00,426.00,24,102.00,112.00,1.74,19.23,48200.00,1928.00,2194.00,20.43,59.87,2142.00,214.20,335.90,4.31,66.80,89.29,1249.00,134,151,104,121)
	lazy val IPE_O_500 = IPESection("IPE O 500",107.00,506.00,202.00,12.00,19.00,21.00,137.00,468.00,426.00,24,104.00,114.00,1.76,16.40,57780.00,2284.00,2613.00,20.56,70.21,2622.00,259.60,408.50,4.38,74.60,143.50,1548.00,114,129,89,104)
	lazy val IPE_A_550 = IPESection("IPE A 550",92.10,547.00,210.00,9.00,15.70,24.00,117.00,515.60,467.60,24,106.00,122.00,1.88,20.36,59980.00,2193.00,2475.00,22.61,60.30,2432.00,231.60,361.50,4.55,68.52,86.53,1710.00,142,160,111,129)
	lazy val IPE_550 = IPESection("IPE 550",106.00,550.00,210.00,11.10,17.20,24.00,134.00,515.60,467.60,24,110.00,122.00,1.88,17.78,67120.00,2441.00,2787.00,22.35,72.34,2668.00,254.10,400.50,4.45,73.62,123.20,1884.00,124,140,97,113)
	lazy val IPE_O_550 = IPESection("IPE O 550",123.00,556.00,212.00,12.70,20.20,24.00,156.00,515.60,467.60,24,110.00,122.00,1.89,15.45,79160.00,2847.00,3263.00,22.52,82.69,3224.00,304.20,480.50,4.55,81.22,187.50,2302.00,108,121,85,98)
	lazy val IPE_A_600 = IPESection("IPE A 600",108.00,597.00,220.00,9.80,17.50,24.00,137.00,562.00,514.00,27,114.00,118.00,2.01,18.72,82920.00,2778.00,3141.00,24.60,70.14,3116.00,283.30,442.10,4.77,72.92,118.80,2607.00,131,147,103,119)
	lazy val IPE_600 = IPESection("IPE 600",122.00,600.00,220.00,12.00,19.00,24.00,156.00,562.00,514.00,27,116.00,118.00,2.02,16.45,92080.00,3069.00,3512.00,24.30,83.78,3387.00,307.90,485.60,4.66,78.12,165.40,2846.00,115,129,91,105)
	lazy val IPE_O_600 = IPESection("IPE O 600",154.00,610.00,224.00,15.00,24.00,24.00,197.00,562.00,514.00,27,118.00,122.00,2.05,13.24,118300.00,3879.00,4471.00,24.52,104.40,4521.00,403.60,640.10,4.79,91.12,318.10,3860.00,93,104,73,85)
	lazy val IPE_750_x_137 = IPESection("IPE 750 x 137",137.00,753.00,263.00,11.50,17.00,17.00,175.00,719.00,685.00,27,102.00,162.00,2.51,18.28,159900.00,4246.00,4865.00,30.26,92.90,5166.00,392.80,614.10,5.44,65.42,137.10,6980.00,128,144,101,116)
	lazy val IPE_750_x_147 = IPESection("IPE 750 x 147",147.00,753.00,265.00,13.20,17.00,17.00,188.00,719.00,685.00,27,104.00,164.00,2.51,17.06,166100.00,4411.00,5110.00,29.76,105.40,5289.00,399.20,630.80,5.31,67.12,161.50,7141.00,120,134,94,109)
	lazy val IPE_750_x_173 = IPESection("IPE 750 x 173",173.00,762.00,267.00,14.40,21.60,17.00,221.00,718.80,684.80,27,104.00,166.00,2.53,14.58,205800.00,5402.00,6218.00,30.49,116.40,6873.00,514.90,809.90,5.57,77.52,273.60,9391.00,102,114,81,93)
	lazy val IPE_750_x_196 = IPESection("IPE 750 x 196",196.00,770.00,268.00,15.60,25.40,17.00,251.00,719.20,685.20,27,106.00,166.00,2.55,12.96,240300.00,6241.00,7174.00,30.95,127.30,8175.00,610.10,958.80,5.71,86.32,408.90,11290.00,91,102,72,83)

	
	
}

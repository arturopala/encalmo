package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.structures.common.section._

/**
* European wide flange beams
* Dim.: HE A, HE B and HE M 100 - 1000 in accordance with Euronorm 53-62; HE AA 100 - 1000; HL 920 - 1100
* Tolerances: EN 10034: 1993
* HE 100 - 900; HE 1000 AA-M; HL AA-R
* A6 - 05 
* HE with GHE>GHE M; HL 920; HL 1000 with GHL>GHL M GHE>GHE M
* Surface condition according to EN 10163-3: 2004, class C, subclass 1
*/
class HESection(name:String) extends IBeamSection(name,"HESection") {

	override def descriptionRef:String = "HESection_description"
}

/**
* HESection factory
*/
object HESection extends Catalog[HESection]("HE Section") {

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
	):HESection = {
		new HESection(p_id){
			h := Number(p_h)
			b := Number(p_b)
			tw := Number(p_tw)
			tf := Number(p_tf)
			hw := Number(p_hw)
			hd := Number(p_hd)
			ss := Number(p_ss)
			phi := Number(p_phi)
			pmin := Number(p_pmin)
			pmax := Number(p_pmax)
			r := Number(p_r)
			A := Number(p_A)
			Iy := Number(p_Iy)
			Iz := Number(p_Iz)
			Wy := Number(p_Wyel)
			Wz := Number(p_Wzel)
			iy := Number(p_iy)
			iz := Number(p_iz)
			m := Number(p_m)
			AL := Number(p_AL)
			AG := Number(p_AG)
			AVz := Number(p_AVz)
			Wypl := Number(p_Wypl)
			Wzpl := Number(p_Wzpl)
			It := Number(p_It)
			Iomega := Number(p_Iw)
			f1 := Number(p_f1)
			f2 := Number(p_f2)
			f3 := Number(p_f3)
			f4 := Number(p_f4)
		}
	}

	/*---------------------------------------
	 *	HESection
	 *---------------------------------------*/

	override val map = Map[String,()=>HESection](
		"HE 100 AA" -> HE_100_AA _,
		"HE 100 A" -> HE_100_A _,
		"HE 100 B" -> HE_100_B _,
		"HE 100 M" -> HE_100_M _,
		"HE 120 AA" -> HE_120_AA _,
		"HE 120 A" -> HE_120_A _,
		"HE 120 B" -> HE_120_B _,
		"HE 120 M" -> HE_120_M _,
		"HE 140 AA" -> HE_140_AA _,
		"HE 140 A" -> HE_140_A _,
		"HE 140 B" -> HE_140_B _,
		"HE 140 M" -> HE_140_M _,
		"HE 160 AA" -> HE_160_AA _,
		"HE 160 A" -> HE_160_A _,
		"HE 160 B" -> HE_160_B _,
		"HE 160 M" -> HE_160_M _,
		"HE 180 AA" -> HE_180_AA _,
		"HE 180 A" -> HE_180_A _,
		"HE 180 B" -> HE_180_B _,
		"HE 180 M" -> HE_180_M _,
		"HE 200 AA" -> HE_200_AA _,
		"HE 200 A" -> HE_200_A _,
		"HE 200 B" -> HE_200_B _,
		"HE 200 M" -> HE_200_M _,
		"HE 220 AA" -> HE_220_AA _,
		"HE 220 A" -> HE_220_A _,
		"HE 220 B" -> HE_220_B _,
		"HE 220 M" -> HE_220_M _,
		"HE 240 AA" -> HE_240_AA _,
		"HE 240 A" -> HE_240_A _,
		"HE 240 B" -> HE_240_B _,
		"HE 240 M" -> HE_240_M _,
		"HE 260 AA" -> HE_260_AA _,
		"HE 260 A" -> HE_260_A _,
		"HE 260 B" -> HE_260_B _,
		"HE 260 M" -> HE_260_M _,
		"HE 280 AA" -> HE_280_AA _,
		"HE 280 A" -> HE_280_A _,
		"HE 280 B" -> HE_280_B _,
		"HE 280 M" -> HE_280_M _,
		"HE 300 AA" -> HE_300_AA _,
		"HE 300 A" -> HE_300_A _,
		"HE 300 B" -> HE_300_B _,
		"HE 300 M" -> HE_300_M _,
		"HE 320 AA" -> HE_320_AA _,
		"HE 320 A" -> HE_320_A _,
		"HE 320 B" -> HE_320_B _,
		"HE 320 M" -> HE_320_M _,
		"HE 340 AA" -> HE_340_AA _,
		"HE 340 A" -> HE_340_A _,
		"HE 340 B" -> HE_340_B _,
		"HE 340 M" -> HE_340_M _,
		"HE 360 AA" -> HE_360_AA _,
		"HE 360 A" -> HE_360_A _,
		"HE 360 B" -> HE_360_B _,
		"HE 360 M" -> HE_360_M _,
		"HE 400 AA" -> HE_400_AA _,
		"HE 400 A" -> HE_400_A _,
		"HE 400 B" -> HE_400_B _,
		"HE 400 M" -> HE_400_M _,
		"HE 450 AA" -> HE_450_AA _,
		"HE 450 A" -> HE_450_A _,
		"HE 450 B" -> HE_450_B _,
		"HE 450 M" -> HE_450_M _,
		"HE 500 AA" -> HE_500_AA _,
		"HE 500 A" -> HE_500_A _,
		"HE 500 B" -> HE_500_B _,
		"HE 500 M" -> HE_500_M _,
		"HE 550 AA" -> HE_550_AA _,
		"HE 550 A" -> HE_550_A _,
		"HE 550 B" -> HE_550_B _,
		"HE 550 M" -> HE_550_M _,
		"HE 600 AA" -> HE_600_AA _,
		"HE 600 A" -> HE_600_A _,
		"HE 600 B" -> HE_600_B _,
		"HE 600 M" -> HE_600_M _,
		"HE 600 x 337" -> HE_600_x_337 _,
		"HE 600 x 399" -> HE_600_x_399 _,
		"HE 650 AA" -> HE_650_AA _,
		"HE 650 A" -> HE_650_A _,
		"HE 650 B" -> HE_650_B _,
		"HE 650 M" -> HE_650_M _,
		"HE 650 x 343" -> HE_650_x_343 _,
		"HE 650 x 407" -> HE_650_x_407 _,
		"HE 700 AA" -> HE_700_AA _,
		"HE 700 A" -> HE_700_A _,
		"HE 700 B" -> HE_700_B _,
		"HE 700 M" -> HE_700_M _,
		"HE 700 x 352" -> HE_700_x_352 _,
		"HE 700 x 418" -> HE_700_x_418 _,
		"HE 800 AA" -> HE_800_AA _,
		"HE 800 A" -> HE_800_A _,
		"HE 800 B" -> HE_800_B _,
		"HE 800 M" -> HE_800_M _,
		"HE 800 x 373" -> HE_800_x_373 _,
		"HE 800 x 444" -> HE_800_x_444 _,
		"HE 900 AA" -> HE_900_AA _,
		"HE 900 A" -> HE_900_A _,
		"HE 900 B" -> HE_900_B _,
		"HE 900 M" -> HE_900_M _,
		"HE 900 x 391" -> HE_900_x_391 _,
		"HE 900 x 466" -> HE_900_x_466 _,
		"HE 1000 AA" -> HE_1000_AA _,
		"HE 1000 x 249" -> HE_1000_x_249 _,
		"HE 1000 A" -> HE_1000_A _,
		"HE 1000 B" -> HE_1000_B _,
		"HE 1000 M" -> HE_1000_M _,
		"HE 1000 x 393" -> HE_1000_x_393 _,
		"HE 1000 x 415" -> HE_1000_x_415 _,
		"HE 1000 x 438" -> HE_1000_x_438 _,
		"HE 1000 x 494" -> HE_1000_x_494 _,
		"HE 1000 x 584" -> HE_1000_x_584 _,
		"HL 920 x 345" -> HL_920_x_345 _,
		"HL 920 x 368" -> HL_920_x_368 _,
		"HL 920 x 390" -> HL_920_x_390 _,
		"HL 920 x 420" -> HL_920_x_420 _,
		"HL 920 x 449" -> HL_920_x_449 _,
		"HL 920 x 491" -> HL_920_x_491 _,
		"HL 920 x 537" -> HL_920_x_537 _,
		"HL 920 x 588" -> HL_920_x_588 _,
		"HL 920 x 656" -> HL_920_x_656 _,
		"HL 920 x 725" -> HL_920_x_725 _,
		"HL 920 x 787" -> HL_920_x_787 _,
		"HL 920 x 970" -> HL_920_x_970 _,
		"HL 1000 AA" -> HL_1000_AA _,
		"HL 1000 A" -> HL_1000_A _,
		"HL 1000 B" -> HL_1000_B _,
		"HL 1000 M" -> HL_1000_M _,
		"HL 1000 x 443" -> HL_1000_x_443 _,
		"HL 1000 x 483" -> HL_1000_x_483 _,
		"HL 1000 x 539" -> HL_1000_x_539 _,
		"HL 1000 x 554" -> HL_1000_x_554 _,
		"HL 1000 x 591" -> HL_1000_x_591 _,
		"HL 1000 x 642" -> HL_1000_x_642 _,
		"HL 1000 x 748" -> HL_1000_x_748 _,
		"HL 1000 x 883" -> HL_1000_x_883 _,
		"HL 1100 A" -> HL_1100_A _,
		"HL 1100 B" -> HL_1100_B _,
		"HL 1100 M" -> HL_1100_M _,
		"HL 1100 R" -> HL_1100_R _
	)

	def HE_100_AA = HESection("HE 100 AA",12.2,91,100,4.2,5.5,12,15.6,80,56,10,54,58,0.553,45.17,236.5,51.98,58.36,3.89,6.15,92.06,18.41,28.44,2.43,29.26,2.51,1.68,290,355,181,245)
	def HE_100_A = HESection("HE 100 A",16.7,96,100,5,8,12,21.2,80,56,10,54,58,0.561,33.68,349.2,72.76,83.01,4.06,7.56,133.8,26.76,41.14,2.51,35.06,5.24,2.58,217,264,138,185)
	def HE_100_B = HESection("HE 100 B",20.4,100,100,6,10,12,26,80,56,10,56,58,0.567,27.76,449.5,89.91,104.2,4.16,9.04,167.3,33.45,51.42,2.53,40.06,9.25,3.38,180,218,115,154)
	def HE_100_M = HESection("HE 100 M",41.8,120,106,12,20,12,53.2,80,56,10,62,64,0.619,14.82,1143,190.4,235.8,4.63,18.04,399.2,75.31,116.3,2.74,66.06,68.21,9.93,96,116,65,85)
	def HE_120_AA = HESection("HE 120 AA",14.6,109,120,4.2,5.5,12,18.6,98,74,12,58,68,0.669,45.94,413.4,75.85,84.12,4.72,6.9,158.8,26.47,40.62,2.93,29.26,2.78,4.24,296,361,182,247)
	def HE_120_A = HESection("HE 120 A",19.9,114,120,5,8,12,25.3,98,74,12,58,68,0.677,34.06,606.2,106.3,119.5,4.89,8.46,230.9,38.48,58.85,3.02,35.06,5.99,6.47,220,267,137,185)
	def HE_120_B = HESection("HE 120 B",26.7,120,120,6.5,11,12,34,98,74,12,60,68,0.686,25.71,864.4,144.1,165.2,5.04,10.96,317.5,52.92,80.97,3.06,42.56,13.84,9.41,167,202,106,141)
	def HE_120_M = HESection("HE 120 M",52.1,140,126,12.5,21,12,66.4,98,74,12,66,74,0.738,14.16,2018,288.2,350.6,5.51,21.15,702.8,111.6,171.6,3.25,68.56,91.66,24.79,92,111,61,80)
	def HE_140_AA = HESection("HE 140 AA",18.1,128,140,4.3,6,12,23,116,92,16,64,76,0.787,43.53,719.5,112.4,123.8,5.59,7.92,274.8,39.26,59.93,3.45,30.36,3.54,10.21,281,342,172,233)
	def HE_140_A = HESection("HE 140 A",24.7,133,140,5.5,8.5,12,31.4,116,92,16,64,76,0.794,32.21,1033,155.4,173.5,5.73,10.12,389.3,55.62,84.85,3.52,36.56,8.13,15.06,208,253,129,174)
	def HE_140_B = HESection("HE 140 B",33.7,140,140,7,12,12,43,116,92,16,66,76,0.805,23.88,1509,215.6,245.4,5.93,13.08,549.7,78.52,119.8,3.58,45.06,20.06,22.48,155,187,98,130)
	def HE_140_M = HESection("HE 140 M",63.2,160,146,13,22,12,80.6,116,92,16,72,82,0.857,13.56,3291,411.4,493.8,6.39,24.46,1144,156.8,240.5,3.77,71.06,120,54.33,88,106,58,76)
	def HE_160_AA = HESection("HE 160 AA",23.8,148,160,4.5,7,15,30.4,134,104,20,76,84,0.901,37.81,1283,173.4,190.4,6.5,10.38,478.7,59.84,91.36,3.97,36.07,6.33,23.75,244,297,150,203)
	def HE_160_A = HESection("HE 160 A",30.4,152,160,6,9,15,38.8,134,104,20,78,84,0.906,29.78,1673,220.1,245.1,6.57,13.21,615.6,76.95,117.6,3.98,41.57,12.19,31.41,192,234,120,161)
	def HE_160_B = HESection("HE 160 B",42.6,160,160,8,13,15,54.3,134,104,20,80,84,0.918,21.56,2492,311.5,354,6.78,17.59,889.2,111.2,170,4.05,51.57,31.24,47.94,140,169,88,118)
	def HE_160_M = HESection("HE 160 M",76.2,180,166,14,23,15,97.1,134,104,20,86,90,0.97,12.74,5098,566.5,674.6,7.25,30.81,1759,211.9,325.5,4.26,77.57,162.4,108.1,83,100,54,71)
	def HE_180_AA = HESection("HE 180 AA",28.7,167,180,5,7.5,15,36.5,152,122,24,84,92,1.018,35.51,1967,235.6,258.2,7.34,12.16,730,81.11,123.6,4.47,37.57,8.33,46.36,229,279,141,190)
	def HE_180_A = HESection("HE 180 A",35.5,171,180,6,9.5,15,45.3,152,122,24,86,92,1.024,28.83,2510,293.6,324.9,7.45,14.47,924.6,102.7,156.5,4.52,42.57,14.8,60.21,187,226,115,155)
	def HE_180_B = HESection("HE 180 B",51.2,180,180,8.5,14,15,65.3,152,122,24,88,92,1.037,20.25,3831,425.7,481.4,7.66,20.24,1363,151.4,231,4.57,54.07,42.16,93.75,131,159,83,110)
	def HE_180_M = HESection("HE 180 M",88.9,200,186,14.5,24,15,113.3,152,122,24,94,98,1.089,12.25,7483,748.3,883.4,8.13,34.65,2580,277.4,425.2,4.77,80.07,203.3,199.3,80,96,52,68)
	def HE_200_AA = HESection("HE 200 AA",34.6,186,200,5.5,8,18,44.1,170,134,27,96,100,1.13,32.62,2944,316.6,347.1,8.17,15.45,1068,106.8,163.2,4.92,42.59,12.69,84.49,211,256,130,175)
	def HE_200_A = HESection("HE 200 A",42.3,190,200,6.5,10,18,53.8,170,134,27,98,100,1.136,26.89,3692,388.6,429.5,8.28,18.08,1336,133.6,203.8,4.98,47.59,20.98,108,174,211,108,145)
	def HE_200_B = HESection("HE 200 B",61.3,200,200,9,15,18,78.1,170,134,27,100,100,1.151,18.78,5696,569.6,642.5,8.54,24.83,2003,200.3,305.8,5.07,60.09,59.28,171.1,122,147,77,102)
	def HE_200_M = HESection("HE 200 M",103,220,206,15,25,18,131.3,170,134,27,106,106,1.203,11.67,10640,967.4,1135,9,41.03,3651,354.5,543.2,5.27,86.09,259.4,346.3,76,92,49,65)
	def HE_220_AA = HESection("HE 220 AA",40.4,205,220,6,8.5,18,51.5,188,152,27,98,118,1.247,30.87,4170,406.9,445.5,9,17.63,1510,137.3,209.3,5.42,44.09,15.93,145.6,200,242,122,165)
	def HE_220_A = HESection("HE 220 A",50.5,210,220,7,11,18,64.3,188,152,27,98,118,1.255,24.85,5410,515.2,568.5,9.17,20.67,1955,177.7,270.6,5.51,50.09,28.46,193.3,161,195,99,134)
	def HE_220_B = HESection("HE 220 B",71.5,220,220,9.5,16,18,91,188,152,27,100,118,1.27,17.77,8091,735.5,827,9.43,27.92,2843,258.5,393.9,5.59,62.59,76.57,295.4,115,140,72,97)
	def HE_220_M = HESection("HE 220 M",117,240,226,15.5,26,18,149.4,188,152,27,108,124,1.322,11.27,14600,1217,1419,9.89,45.31,5012,443.5,678.6,5.79,88.59,315.3,572.7,73,88,47,62)
	def HE_240_AA = HESection("HE 240 AA",47.4,224,240,6.5,9,21,60.4,206,164,27,104,138,1.359,28.67,5835,521,570.6,9.83,21.54,2077,173.1,264.4,5.87,49.1,22.98,239.6,185,225,114,154)
	def HE_240_A = HESection("HE 240 A",60.3,230,240,7.5,12,21,76.8,206,164,27,104,138,1.369,22.7,7763,675.1,744.6,10.05,25.18,2769,230.7,351.7,6,56.1,41.55,328.5,147,178,91,122)
	def HE_240_B = HESection("HE 240 B",83.2,240,240,10,17,21,106,206,164,27,108,138,1.384,16.63,11260,938.3,1053,10.31,33.23,3923,326.9,498.4,6.08,68.6,102.7,486.9,108,131,68,91)
	def HE_240_M = HESection("HE 240 M",157,270,248,18,32,21,199.6,206,164,27,116,146,1.46,9.318,24290,1799,2117,11.03,60.07,8153,657.5,1006,6.39,106.6,627.9,1152,61,73,39,52)
	def HE_260_AA = HESection("HE 260 AA",54.1,244,260,6.5,9.5,24,69,225,177,27,110,158,1.474,27.22,7981,654.1,714.5,10.76,24.75,2788,214.5,327.7,6.36,53.62,30.31,382.6,176,214,108,146)
	def HE_260_A = HESection("HE 260 A",68.2,250,260,7.5,12.5,24,86.8,225,177,27,110,158,1.484,21.77,10450,836.4,919.8,10.97,28.76,3668,282.1,430.2,6.5,60.62,52.37,516.4,141,171,88,117)
	def HE_260_B = HESection("HE 260 B",93,260,260,10,17.5,24,118.4,225,177,27,114,158,1.499,16.12,14920,1148,1283,11.22,37.59,5135,395,602.2,6.58,73.12,123.8,753.7,105,127,66,88)
	def HE_260_M = HESection("HE 260 M",172,290,268,18,32.5,24,219.6,225,177,27,122,166,1.575,9.133,31310,2159,2524,11.94,66.89,10450,779.7,1192,6.9,111.1,719,1728,59,72,39,51)
	def HE_280_AA = HESection("HE 280 AA",61.2,264,280,7,10,24,78,244,196,27,110,178,1.593,26.01,10560,799.8,873.1,11.63,27.52,3664,261.7,399.4,6.85,55.12,36.22,590.1,168,204,104,139)
	def HE_280_A = HESection("HE 280 A",76.4,270,280,8,13,24,97.3,244,196,27,112,178,1.603,20.99,13670,1013,1112,11.86,31.74,4763,340.2,518.1,7,62.12,62.1,785.4,136,165,84,113)
	def HE_280_B = HESection("HE 280 B",103,280,280,10.5,18,24,131.4,244,196,27,114,178,1.618,15.69,19270,1376,1534,12.11,41.09,6595,471,717.6,7.09,74.62,143.7,1130,102,123,64,85)
	def HE_280_M = HESection("HE 280 M",189,310,288,18.5,33,24,240.2,244,196,27,122,186,1.694,8.984,39550,2551,2966,12.83,72.03,13160,914.1,1397,7.4,112.6,807.3,2520,59,71,38,50)
	def HE_300_AA = HESection("HE 300 AA",69.8,283,300,7.5,10.5,27,88.9,262,208,27,116,198,1.705,24.42,13800,975.6,1065,12.46,32.37,4734,315.6,482.3,7.3,60.13,49.35,877.2,158,192,97,131)
	def HE_300_A = HESection("HE 300 A",88.3,290,300,8.5,14,27,112.5,262,208,27,118,198,1.717,19.43,18260,1260,1383,12.74,37.28,6310,420.6,641.2,7.49,68.13,85.17,1200,126,153,78,105)
	def HE_300_B = HESection("HE 300 B",117,300,300,11,19,27,149.1,262,208,27,120,198,1.732,14.8,25170,1678,1869,12.99,47.43,8563,570.9,870.1,7.58,80.63,185,1688,96,116,60,80)
	def HE_300_M = HESection("HE 300 M",238,340,310,21,39,27,303.1,262,208,27,132,208,1.832,7.699,59200,3482,4078,13.98,90.53,19400,1252,1913,8,130.6,1408,4386,50,60,33,43)
	def HE_320_AA = HESection("HE 320 AA",74.2,301,300,8,11,27,94.6,279,225,27,118,198,1.74,23.43,16450,1093,1196,13.19,35.4,4959,330.6,505.7,7.24,61.63,55.87,1041,152,184,95,127)
	def HE_320_A = HESection("HE 320 A",97.6,310,300,9,15.5,27,124.4,279,225,27,118,198,1.756,17.98,22930,1479,1628,13.58,41.13,6985,465.7,709.7,7.49,71.63,108,1512,117,141,74,98)
	def HE_320_B = HESection("HE 320 B",127,320,300,11.5,20.5,27,161.3,279,225,27,122,198,1.771,13.98,30820,1926,2149,13.82,51.77,9239,615.9,939.1,7.57,84.13,225.1,2069,91,110,58,77)
	def HE_320_M = HESection("HE 320 M",245,359,309,21,40,27,312,279,225,27,132,204,1.866,7.616,68130,3796,4435,14.78,94.85,19710,1276,1951,7.95,132.6,1501,5004,50,60,33,43)
	def HE_340_AA = HESection("HE 340 AA",78.9,320,300,8.5,11.5,27,100.5,297,243,27,118,198,1.777,22.52,19550,1222,1341,13.95,38.69,5185,345.6,529.3,7.18,63.13,63.07,1231,147,177,94,123)
	def HE_340_A = HESection("HE 340 A",105,330,300,9.5,16.5,27,133.5,297,243,27,118,198,1.795,17.13,27690,1678,1850,14.4,44.95,7436,495.7,755.9,7.46,74.13,127.2,1824,112,134,72,94)
	def HE_340_B = HESection("HE 340 B",134,340,300,12,21.5,27,170.9,297,243,27,122,198,1.81,13.49,36660,2156,2408,14.65,56.09,9690,646,985.7,7.53,86.63,257.2,2454,88,106,57,75)
	def HE_340_M = HESection("HE 340 M",248,377,309,21,40,27,315.8,297,243,27,132,204,1.902,7.67,76370,4052,4718,15.55,98.63,19710,1276,1953,7.9,132.6,1506,5584,50,60,34,43)
	def HE_360_AA = HESection("HE 360 AA",83.7,339,300,9,12,27,106.6,315,261,27,118,198,1.814,21.67,23040,1359,1495,14.7,42.17,5410,360.7,553,7.12,64.63,70.99,1444,142,170,92,120)
	def HE_360_A = HESection("HE 360 A",112,350,300,10,17.5,27,142.8,315,261,27,120,198,1.834,16.36,33090,1891,2088,15.22,48.96,7887,525.8,802.3,7.43,76.63,148.8,2177,107,128,70,91)
	def HE_360_B = HESection("HE 360 B",142,360,300,12.5,22.5,27,180.6,315,261,27,122,198,1.849,13.04,43190,2400,2683,15.46,60.6,10140,676.1,1032,7.49,89.13,292.5,2883,86,102,56,73)
	def HE_360_M = HESection("HE 360 M",250,395,308,21,40,27,318.8,315,261,27,132,204,1.934,7.73,84870,4297,4989,16.32,102.4,19520,1268,1942,7.83,132.6,1507,6137,51,61,34,44)
	def HE_400_AA = HESection("HE 400 AA",92.4,378,300,9.5,13,27,117.7,352,298,27,118,198,1.891,20.46,31250,1654,1824,16.3,47.95,5861,390.8,599.7,7.06,67.13,84.69,1948,135,161,90,115)
	def HE_400_A = HESection("HE 400 A",125,390,300,11,19,27,159,352,298,27,120,198,1.912,15.32,45070,2311,2562,16.84,57.33,8564,570.9,872.9,7.34,80.63,189,2942,101,120,68,87)
	def HE_400_B = HESection("HE 400 B",155,400,300,13.5,24,27,197.8,352,298,27,124,198,1.927,12.41,57680,2884,3232,17.08,69.98,10820,721.3,1104,7.4,93.13,355.7,3817,82,97,56,71)
	def HE_400_M = HESection("HE 400 M",256,432,307,21,40,27,325.8,352,298,27,132,202,2.004,7.835,104100,4820,5571,17.88,110.2,19340,1260,1934,7.7,132.6,1515,7410,52,62,36,45)
	def HE_450_AA = HESection("HE 450 AA",99.7,425,300,10,13.5,27,127.1,398,344,27,120,198,1.984,19.89,41890,1971,2183,18.16,54.7,6088,405.8,624.4,6.92,68.63,95.61,2572,133,156,91,114)
	def HE_450_A = HESection("HE 450 A",140,440,300,11.5,21,27,178,398,344,27,122,198,2.011,14.39,63720,2896,3216,18.92,65.78,9465,631,965.5,7.29,85.13,243.8,4148,96,113,66,83)
	def HE_450_B = HESection("HE 450 B",171,450,300,14,26,27,218,398,344,27,124,198,2.026,11.84,79890,3551,3982,19.14,79.66,11720,781.4,1198,7.33,97.63,440.5,5258,79,93,55,69)
	def HE_450_M = HESection("HE 450 M",263,478,307,21,40,27,335.4,398,344,27,132,202,2.096,7.959,131500,5501,6331,19.8,119.8,19340,1260,1939,7.59,132.6,1529,9251,53,62,38,47)
	def HE_500_AA = HESection("HE 500 AA",107,472,300,10.5,14,27,136.9,444,390,27,120,198,2.077,19.33,54640,2315,2576,19.98,61.91,6314,420.9,649.3,6.79,70.13,107.7,3304,130,152,91,113)
	def HE_500_A = HESection("HE 500 A",155,490,300,12,23,27,197.5,444,390,27,122,198,2.11,13.6,86970,3550,3949,20.98,74.72,10370,691.1,1059,7.24,89.63,309.3,5643,92,107,65,80)
	def HE_500_B = HESection("HE 500 B",187,500,300,14.5,28,27,238.6,444,390,27,124,198,2.125,11.34,107200,4287,4815,21.19,89.82,12620,841.6,1292,7.27,102.1,538.4,7018,76,89,54,67)
	def HE_500_M = HESection("HE 500 M",270,524,306,21,40,27,344.3,444,390,27,132,202,2.184,8.079,161900,6180,7094,21.69,129.5,19150,1252,1932,7.46,132.6,1539,11190,55,63,39,48)
	def HE_550_AA = HESection("HE 550 AA",120,522,300,11.5,15,27,152.8,492,438,27,122,198,2.175,18.13,72870,2792,3128,21.84,72.66,6767,451.1,698.6,6.65,73.13,133.7,4338,123,142,88,108)
	def HE_550_A = HESection("HE 550 A",166,540,300,12.5,24,27,211.8,492,438,27,122,198,2.209,13.29,111900,4146,4622,22.99,83.72,10820,721.3,1107,7.15,92.13,351.5,7189,90,104,65,79)
	def HE_550_B = HESection("HE 550 B",199,550,300,15,29,27,254.1,492,438,27,124,198,2.224,11.15,136700,4971,5591,23.2,100.1,13080,871.8,1341,7.17,104.6,600.3,8856,76,88,55,67)
	def HE_550_M = HESection("HE 550 M",278,572,306,21,40,27,354.4,492,438,27,132,202,2.28,8.195,198000,6923,7933,23.64,139.6,19160,1252,1937,7.35,132.6,1554,13520,56,64,41,50)
	def HE_600_AA = HESection("HE 600 AA",129,571,300,12,15.5,27,164.1,540,486,27,122,198,2.272,17.64,91900,3218,3623,23.66,81.29,6993,466.2,724.5,6.53,74.63,149.8,5381,120,138,88,106)
	def HE_600_A = HESection("HE 600 A",178,590,300,13,25,27,226.5,540,486,27,122,198,2.308,12.98,141200,4787,5350,24.97,93.21,11270,751.4,1156,7.05,94.63,397.8,8978,89,102,65,79)
	def HE_600_B = HESection("HE 600 B",212,600,300,15.5,30,27,270,540,486,27,126,198,2.323,10.96,171000,5701,6425,25.17,110.8,13530,902,1391,7.08,107.1,667.2,10970,75,86,56,67)
	def HE_600_M = HESection("HE 600 M",285,620,305,21,40,27,363.7,540,486,27,132,200,2.372,8.308,237400,7660,8772,25.55,149.7,18980,1244,1930,7.22,132.6,1564,15910,57,65,42,51)
	def HE_600_x_337 = HESection("HE 600 x 337",337,632,310,25.5,46,27,429.2,540,486,27,138,202,2.407,7.144,283200,8961,10380,25.69,180.5,22940,1480,2310,7.31,149.1,2451,19610,49,56,37,44)
	def HE_600_x_399 = HESection("HE 600 x 399",399,648,315,30,54,27,508.5,540,486,27,142,208,2.45,6.137,344600,10640,12460,26.03,213.6,28280,1796,2814,7.46,169.6,3966,24810,42,48,32,38)
	def HE_650_AA = HESection("HE 650 AA",138,620,300,12.5,16,27,175.8,588,534,27,122,198,2.369,17.17,113900,3676,4160,25.46,90.4,7221,481.4,750.7,6.41,76.13,167.5,6567,118,135,88,105)
	def HE_650_A = HESection("HE 650 A",190,640,300,13.5,26,27,241.6,588,534,27,124,198,2.407,12.69,175200,5474,6136,26.93,103.2,11720,781.6,1205,6.97,97.13,448.3,11030,87,100,65,78)
	def HE_650_B = HESection("HE 650 B",225,650,300,16,31,27,286.3,588,534,27,126,198,2.422,10.77,210600,6480,7320,27.12,122,13980,932.3,1441,6.99,109.6,739.2,13360,74,85,56,66)
	def HE_650_M = HESection("HE 650 M",293,668,305,21,40,27,373.7,588,534,27,132,200,2.468,8.411,281700,8433,9657,27.45,159.7,18980,1245,1936,7.13,132.6,1579,18650,58,66,44,52)
	def HE_650_x_343 = HESection("HE 650 x 343",343,680,309,25,46,27,437.5,588,534,27,138,202,2.5,7.278,333700,9815,11350,27.62,189.6,22720,1470,2300,7.21,148.6,2442,22730,50,57,38,45)
	def HE_650_x_407 = HESection("HE 650 x 407",407,696,314,29.5,54,27,518.8,588,534,27,142,206,2.543,6.243,405400,11650,13620,27.95,224.8,28020,1785,2803,7.35,169.1,3958,28710,43,49,33,39)
	def HE_700_AA = HESection("HE 700 AA",150,670,300,13,17,27,190.9,636,582,27,122,198,2.468,16.46,142700,4260,4840,27.34,100.3,7673,511.5,799.7,6.34,78.63,195.2,8155,114,129,86,102)
	def HE_700_A = HESection("HE 700 A",204,690,300,14.5,27,27,260.5,636,582,27,124,198,2.505,12.25,215300,6241,7032,28.75,117,12180,811.9,1257,6.84,100.1,513.9,13350,85,96,64,76)
	def HE_700_B = HESection("HE 700 B",241,700,300,17,32,27,306.4,636,582,27,126,198,2.52,10.48,256900,7340,8327,28.96,137.1,14440,962.7,1495,6.87,112.6,830.9,16060,72,82,55,65)
	def HE_700_M = HESection("HE 700 M",301,716,304,21,40,27,383,636,582,27,132,200,2.56,8.513,329300,9198,10540,29.32,169.8,18800,1237,1929,7.01,132.6,1589,21400,59,67,45,53)
	def HE_700_x_352 = HESection("HE 700 x 352",352,728,308,25,46,27,448.6,636,582,27,138,200,2.592,7.359,389700,10710,12390,29.47,201.6,22510,1461,2293,7.08,148.6,2461,26050,51,58,39,46)
	def HE_700_x_418 = HESection("HE 700 x 418",418,744,313,29.5,54,27,531.9,636,582,27,142,206,2.635,6.31,472500,12700,14840,29.8,239,27760,1774,2797,7.22,169.1,3989,32850,44,50,34,40)
	def HE_800_AA = HESection("HE 800 AA",172,770,300,14,18,30,218.5,734,674,27,130,198,2.66,15.51,208900,5426,6225,30.92,123.8,8134,542.2,856.6,6.1,85.15,256.8,11450,108,122,84,98)
	def HE_800_A = HESection("HE 800 A",224,790,300,15,28,30,285.8,734,674,27,130,198,2.698,12.03,303400,7682,8699,32.58,138.8,12640,842.6,1312,6.65,106.1,596.9,18290,84,94,66,76)
	def HE_800_B = HESection("HE 800 B",262,800,300,17.5,33,30,334.2,734,674,27,134,198,2.713,10.34,359100,8977,10230,32.78,161.8,14900,993.6,1553,6.68,118.6,946,21840,72,81,57,66)
	def HE_800_M = HESection("HE 800 M",317,814,303,21,40,30,404.3,734,674,27,138,198,2.746,8.655,442600,10870,12490,33.09,194.3,18630,1230,1930,6.79,136.1,1646,27780,60,68,48,55)
	def HE_800_x_373 = HESection("HE 800 x 373",373,826,308,25,46,30,474.6,734,674,27,144,200,2.782,7.469,523900,12690,14700,33.23,230.3,22530,1463,2311,6.89,152.1,2554,34070,52,59,41,48)
	def HE_800_x_444 = HESection("HE 800 x 444",444,842,313,30,54,30,566,734,674,27,148,206,2.824,6.357,634500,15070,17640,33.48,276.5,27800,1776,2827,7.01,173.1,4180,42840,44,50,35,41)
	def HE_900_AA = HESection("HE 900 AA",198,870,300,15,20,30,252.2,830,770,27,130,198,2.858,14.44,301100,6923,7999,34.55,147.2,9041,602.8,957.7,5.99,90.15,334.9,16260,101,113,81,93)
	def HE_900_A = HESection("HE 900 A",252,890,300,16,30,30,320.5,830,770,27,132,198,2.896,11.51,422100,9485,10810,36.29,163.3,13550,903.2,1414,6.5,111.1,736.8,24960,81,90,65,74)
	def HE_900_B = HESection("HE 900 B",291,900,300,18.5,35,30,371.3,830,770,27,134,198,2.911,9.99,494100,10980,12580,36.48,188.8,15820,1054,1658,6.53,123.6,1137,29460,70,78,57,65)
	def HE_900_M = HESection("HE 900 M",333,910,302,21,40,30,423.6,830,770,27,138,198,2.934,8.824,570400,12540,14440,36.7,214.4,18450,1222,1929,6.6,136.1,1671,34750,62,69,50,57)
	def HE_900_x_391 = HESection("HE 900 x 391",391,922,307,25,46,30,497.7,830,770,27,144,200,2.97,7.604,674300,14630,16990,36.81,254.3,22320,1454,2312,6.7,152.1,2597,42560,54,60,43,49)
	def HE_900_x_466 = HESection("HE 900 x 466",466,938,312,30,54,30,593.7,830,770,27,148,204,3.012,6.464,814900,17380,20380,37.05,305.3,27560,1767,2832,6.81,173.1,4256,53400,45,51,37,42)
	def HE_1000_AA = HESection("HE 1000 AA",222,970,300,16,21,30,282.2,928,868,27,132,198,3.056,13.8,406500,8380,9777,37.95,172.2,9501,633.4,1016,5.8,93.15,403.4,21280,98,108,79,90)
	def HE_1000_x_249 = HESection("HE 1000 x 249",249,980,300,16.5,26,30,316.8,928,868,27,134,194,3.08,12.37,481100,9818,11350,38.97,180.7,11750,784,1245,6.09,103.6,584.4,26620,88,97,71,81)
	def HE_1000_A = HESection("HE 1000 A",272,990,300,16.5,31,30,346.8,928,868,27,132,198,3.095,11.37,553800,11190,12820,39.96,184.6,14000,933.6,1470,6.35,113.6,822.4,32070,81,89,66,74)
	def HE_1000_B = HESection("HE 1000 B",314,1000,300,19,36,30,400,928,868,27,134,198,3.11,9.905,644700,12890,14860,40.15,212.5,16280,1085,1716,6.38,126.1,1254,37640,70,78,57,65)
	def HE_1000_M = HESection("HE 1000 M",349,1008,302,21,40,30,444.2,928,868,27,138,198,3.13,8.978,722300,14330,16570,40.32,235,18460,1222,1940,6.45,136.1,1701,43020,64,70,52,59)
	def HE_1000_x_393 = HESection("HE 1000 x 393",393,1016,303,24.4,43.9,30,500.2,928,868,27,142,198,3.14,8.01,807700,15900,18540,40.18,271.3,20500,1353,2168,6.4,147.3,2332,48080,57,63,47,53)
	def HE_1000_x_415 = HESection("HE 1000 x 415",415,1020,304,26,46,30,528.7,928,868,27,144,198,3.15,7.6,853100,16728,19571,40.17,288.6,21710,1428,2298,6.41,153.1,2713,51080,54,60,44,50)
	def HE_1000_x_438 = HESection("HE 1000 x 438",437,1026,305,26.9,49,30,557.2,928,868,27,146,198,3.17,7.24,909800,17740,20770,40.41,300.9,23360,1532,2464,6.47,160.1,3200,55290,51,57,42,48)
	def HE_1000_x_494 = HESection("HE 1000 x 494",494,1036,309,31,54,30,629.1,928,868,27,148,204,3.19,6.47,1028000,19845,23413,40.42,344.5,26820,1736,2818,6.53,174.1,4433,64010,46,51,38,43)
	def HE_1000_x_584 = HESection("HE 1000 x 584",584,1056,314,36,64,30,743.7,928,868,27,154,208,3.24,5.56,1246100,23600,28039,40.93,403.2,33430,2130,3475,6.7,199.1,7230,81240,39,44,33,37)
	def HL_920_x_345 = HESection("HL 920 x 345",345,927,418,19.3,32,19,437.2,863,825,27,126,312,3.45,10.07,645000,13920,15700,38.41,188,39010,1867,2880,9.45,105.6,1159,78120,69,79,52,62)
	def HL_920_x_368 = HESection("HL 920 x 368",368,931,419,20.3,34.3,19,465.6,862.4,824.4,27,128,314,3.46,9.48,692200,14870,16790,38.56,198.2,42120,2010,3104,9.51,111.2,1408,84670,65,74,49,58)
	def HL_920_x_390 = HESection("HL 920 x 390",390,936,420,21.3,36.6,19,494.3,862.8,824.8,27,128,314,3.48,8.96,741700,15850,17920,38.74,208.6,45270,2156,3331,9.57,116.8,1691,91550,62,70,46,55)
	def HL_920_x_420 = HESection("HL 920 x 420",420,943,422,22.5,39.9,19,534.1,863.2,825.2,27,130,316,3.5,8.34,813300,17250,19530,39.02,221.5,50070,2373,3667,9.68,124.6,2151,102100,58,66,43,51)
	def HL_920_x_449 = HESection("HL 920 x 449",449,948,423,24,42.7,19,571.4,862.6,824.6,27,130,318,3.51,7.82,874700,18450,20950,39.13,236.6,53970,2552,3949,9.72,131.7,2627,110600,54,61,41,48)
	def HL_920_x_491 = HESection("HL 920 x 491",491,957,422,25.9,47,19,623.3,863,825,27,132,316,3.52,7.19,966300,20200,23000,39.37,256.6,59000,2796,4335,9.73,142.2,3441,122200,50,56,37,44)
	def HL_920_x_537 = HESection("HL 920 x 537",537,965,425,28.4,51.1,19,682.5,862.8,824.8,27,136,320,3.54,6.61,1066000,22080,25270,39.51,282.1,65550,3085,4795,9.8,152.9,4447,136900,46,52,35,41)
	def HL_920_x_588 = HESection("HL 920 x 588",588,976,427,31,55.9,19,748.4,864.2,826.2,27,138,322,3.57,6.07,1184000,24260,27880,39.78,309.6,72760,3408,5310,9.86,165.1,5808,154000,42,48,32,37)
	def HL_920_x_656 = HESection("HL 920 x 656",656,987,431,34.5,62,19,835.3,863,825,27,144,320,3.6,5.48,1335000,27060,31270,39.98,345.8,83040,3853,6022,9.97,180.8,7950,177600,38,43,29,34)
	def HL_920_x_725 = HESection("HL 920 x 725",725,999,434,38.1,68.1,19,922.9,862.8,824.8,27,148,323,3.63,5,1492000,29880,34740,40.21,383.6,93200,4295,6734,10.05,196.6,10570,201900,35,39,26,31)
	def HL_920_x_787 = HESection("HL 920 x 787",787,1011,437,40.9,73.9,19,1002,863.2,825.2,27,152,326,3.66,4.65,1646000,32560,38010,40.53,414.5,103300,4728,7425,10.15,211,13430,226800,32,37,25,29)
	def HL_920_x_970 = HESection("HL 920 x 970",970,1043,446,50,89.9,19,1236.6,863.2,825.2,27,160,334,3.74,3.85,2100000,40270,47660,41.21,513.8,133900,6002,9490,10.4,252.1,24320,304000,27,30,20,24)
	def HL_1000_AA = HESection("HL 1000 AA",296,982,400,16.5,27,30,376.8,928,868,27,134,294,3.479,11.76,618700,12600,14220,40.52,181.5,28850,1443,2235,8.75,105.6,756.9,65670,82,92,63,73)
	def HL_1000_A = HESection("HL 1000 A",321,990,400,16.5,31,30,408.8,928,868,27,134,294,3.495,10.89,696400,14070,15800,41.27,184.6,33120,1656,2555,9,113.6,1021,76030,76,85,58,68)
	def HL_1000_B = HESection("HL 1000 B",371,1000,400,19,36,30,472,928,868,27,136,294,3.51,9.474,812100,16240,18330,41.48,212.5,38480,1924,2976,9.03,126.1,1565,89210,66,74,51,59)
	def HL_1000_M = HESection("HL 1000 M",412,1008,402,21,40,30,524.2,928,868,27,142,290,3.53,8.58,909800,18050,20440,41.66,235,43410,2160,3348,9.1,136.1,2128,101460,60,67,46,54)
	def HL_1000_x_443 = HESection("HL 1000 x 443",443,1012,402,23.6,41.9,30,563.7,928,868,27,142,296,3.53,7.99,966510,19101,21777,41.41,261.8,45500,2264,3529,8.98,142.5,2545,106740,55,63,43,50)
	def HL_1000_x_483 = HESection("HL 1000 x 483",483,1020,404,25.4,46,30,615.1,928,868,27,144,298,3.55,7.36,1067480,20931,23923,41.66,282.7,50710,2510,3919,9.08,152.5,3311,119900,51,58,40,46)
	def HL_1000_x_539 = HESection("HL 1000 x 539",539,1030,407,28.4,51.1,30,687.2,928,868,27,146,302,3.58,6.64,1202540,23350,26824,41.83,316.4,57630,2832,4436,9.16,165.7,4546,137550,46,52,36,42)
	def HL_1000_x_554 = HESection("HL 1000 x 554",554,1032,408,29.5,52,30,705.8,928,868,27,150,296,3.59,6.47,1232000,23880,27500,41.79,328,59100,2897,4547,9.15,168.6,4860,141330,45,51,35,41)
	def HL_1000_x_591 = HESection("HL 1000 x 591",591,1040,409,31,55.9,30,752.7,928,868,27,148,304,3.6,6.1,1331040,25597,29530,42.05,346.3,64010,3130,4916,9.22,177.9,5927,154330,42,48,33,39)
	def HL_1000_x_642 = HESection("HL 1000 x 642",642,1048,412,34,60,30,817.6,928,868,27,154,300,3.62,5.65,1451000,27680,32100,42.12,379.6,70280,3412,5379,9.27,189.1,7440,170670,39,44,31,36)
	def HL_1000_x_748 = HESection("HL 1000 x 748",748,1068,417,39,70,30,953.4,928,868,27,160,304,3.67,4.91,1732000,32430,37880,42.62,438.9,85111,4082,6459,9.45,214.1,11670,210650,34,38,27,31)
	def HL_1000_x_883 = HESection("HL 1000 x 883",883,1092,424,45.5,82,30,1125.3,928,868,27,166,312,3.74,4.23,2096000,38390,45260,43.16,516.5,105000,4952,7874,9.66,244.6,18750,265670,29,33,23,27)
	def HL_1100_A = HESection("HL 1100 A",343,1090,400,18,31,20,436.5,1028,988,27,116,294,3.71,10.83,867400,15920,18060,44.58,206.5,33120,1656,2568,8.71,103.4,1037,92710,76,85,59,68)
	def HL_1100_B = HESection("HL 1100 B",390,1100,400,20,36,20,497,1028,988,27,118,294,3.726,9.549,1005000,18280,20780,44.98,230.6,38480,1924,2988,8.8,115.4,1564,108680,67,75,52,60)
	def HL_1100_M = HESection("HL 1100 M",433,1108,402,22,40,20,551.2,1028,988,27,122,290,3.746,8.657,1126000,20320,23160,45.19,254.4,43410,2160,3362,8.87,125.4,2130,123500,61,68,47,55)
	def HL_1100_R = HESection("HL 1100 R",499,1118,405,26,45,20,635.2,1028,988,27,126,294,3.77,7.56,1294000,23150,26600,45.14,300.4,49980,2468,3870,8.87,139.4,3135,143410,53,59,42,48)




}

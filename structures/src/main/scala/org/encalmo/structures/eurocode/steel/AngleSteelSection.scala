package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{Evaluate, Text, NumSection}

trait AngleSteelSectionSymbols extends SymbolConfigurator {

    val angleDict = "AngleSteelSection"

    //Dimensions of details
    val t = symbol(BasicSymbols.t) unit "mm" dict angleDict
    val r1 = symbol(BasicSymbols.r|1) unit "mm" dict angleDict
    val r2 = symbol(BasicSymbols.r|2) unit "mm" dict angleDict
    val ys = symbol(BasicSymbols.y|BasicSymbols.s) unit "mm" dict angleDict
    val zs = symbol(BasicSymbols.r|BasicSymbols.s) unit "mm" dict angleDict
    val v1 = symbol(BasicSymbols.v|1) unit "mm" dict angleDict
    val v2 = symbol(BasicSymbols.v|2) unit "mm" dict angleDict
    val u1 = symbol(BasicSymbols.u|1) unit "mm" dict angleDict
    val u2 = symbol(BasicSymbols.u|2) unit "mm" dict angleDict
    val u3 = symbol(BasicSymbols.u|3) unit "mm" dict angleDict
    val emin = symbol(BasicSymbols.e|"min") unit "mm" dict angleDict
    val emax = symbol(BasicSymbols.e|"max") unit "mm" dict angleDict
    val Anet = symbol(BasicSymbols.A|"net") unit "cm2" dict angleDict
        
}

/** Section's shape trait */
class AngleSteelSection(name:String, val sectionType: String) extends SteelSection(name) with AngleSteelSectionSymbols {

    Wzd := Wz
    Wzg := Wz
    Wyd := Wy
    Wyg := Wy

    epsi := 0.814

    C1 := Number(4) unless (IsLessThanOrEqualTo(h/t,15*epsi) and IsLessThanOrEqualTo((b+h)/(2*t),11.5*epsi) thenUse Number(3))

    wy := text("b")
    wz := text("b")

	def info = NumSection(Text(sectionType,angleDict),name,
		Evaluate(h,b,t,r1,r2,ys)
	)
	
}

class AngleEqualLegSection(name:String) extends AngleSteelSection(name,"Angle Equal Leg") {

    override def descriptionRef:String = "LESection_description"
}

object AngleEqualLegSection extends Catalog[AngleEqualLegSection]("Angle Equal Leg")  {

    def apply(
        name:String,
        p_m:Double,
        p_h:Double,
        p_t:Double,
        p_r1:Double,
        p_r2:Double,
        p_A:Double,
        p_ys:Double,
        p_v:Double,
        p_u1:Double,
        p_u2:Double,
        p_AL:Double,
        p_AG:Double,
        p_Iy:Double,
        p_Wy:Double,
        p_iy:Double,
        p_Iyz:Double,
        p_Anet:Double,
        p_phi:Double,
        p_emin:Double,
        p_emax:Double,
        p_Iu:Double,
        p_iu:Double,
        p_Iv:Double,
        p_iv:Double
     ):AngleEqualLegSection = new AngleEqualLegSection(name) {
        h := p_h
        b := p_h
        t := p_t
        r1 := p_r1
        r2 := p_r2
        ys := p_ys
        zs := p_ys
        v1 := p_v
        v2 := p_v
        u1 := p_u1
        u2 := p_u2
        u3 := p_u2
        alpha := 45
        A := p_A
        Iy := p_Iy
        Iz := Iy
        Iyz := p_Iyz
        Wy := p_Wy
        Wz := Wy
        Wypl := Wy
        Wzpl := Wz
        iy := p_iy
        iz := iy
        m := p_m
        AL := p_AL
        AG := p_AG
        Anet := p_Anet
        emin := p_emin
        emax := p_emax
        phi := p_phi
        /*p_Iu
        p_iu
        p_Iv
        p_iv*/
    }

    val map = Map[String,()=>AngleEqualLegSection](
        "L 90 x 90 x 7" -> L_90_x_90_x_7 _,
        "L 90 x 90 x 8" -> L_90_x_90_x_8 _,
        "L 90 x 90 x 9" -> L_90_x_90_x_9 _,
        "L 100 x 100 x 8" -> L_100_x_100_x_8 _,
        "L 100 x 100 x 10" -> L_100_x_100_x_10 _,
        "L 100 x 100 x 12" -> L_100_x_100_x_12 _,
        "L 110 x 110 x 10" -> L_110_x_110_x_10 _,
        "L 110 x 110 x 12" -> L_110_x_110_x_12 _,
        "L 120 x 120 x 10" -> L_120_x_120_x_10 _,
        "L 120 x 120 x 11" -> L_120_x_120_x_11 _,
        "L 120 x 120 x 12" -> L_120_x_120_x_12 _,
        "L 120 x 120 x 13" -> L_120_x_120_x_13 _,
        "L 120 x 120 x 15" -> L_120_x_120_x_15 _,
        "L 130 x 130 x 12" -> L_130_x_130_x_12 _,
        "L 140 x 140 x 10" -> L_140_x_140_x_10 _,
        "L 140 x 140 x 13" -> L_140_x_140_x_13 _,
        "L 150 x 150 x 10" -> L_150_x_150_x_10 _,
        "L 150 x 150 x 12" -> L_150_x_150_x_12 _,
        "L 150 x 150 x 14" -> L_150_x_150_x_14 _,
        "L 150 x 150 x 15" -> L_150_x_150_x_15 _,
        "L 150 x 150 x 18" -> L_150_x_150_x_18 _,
        "L 160 x 160 x 15" -> L_160_x_160_x_15 _,
        "L 160 x 160 x 17" -> L_160_x_160_x_17 _,
        "L 180 x 180 x 16" -> L_180_x_180_x_16 _,
        "L 180 x 180 x 18" -> L_180_x_180_x_18 _,
        "L 200 x 200 x 16" -> L_200_x_200_x_16 _,
        "L 200 x 200 x 18" -> L_200_x_200_x_18 _,
        "L 200 x 200 x 20" -> L_200_x_200_x_20 _,
        "L 200 x 200 x 24" -> L_200_x_200_x_24 _
    )

    def L_90_x_90_x_7    = AngleEqualLegSection(name="L 90 x 90 x 7"     , p_m=    9.6, p_h=     90, p_t=      7, p_r1=     11, p_r2=    5.5, p_A=  12.24, p_ys=   2.45, p_v=   6.36, p_u1=   3.47, p_u2=   3.16, p_AL=  0.351, p_AG=  36.48, p_Iy=   92.5, p_Wy=  14.13, p_iy=   2.75, p_Iu=  147.1, p_iu=   3.47, p_Iv=  38.02, p_iv=   1.76, p_Iyz= -54.53, p_phi=  24.00, p_emin=     47, p_emax=     51, p_Anet=  10.42)
    def L_90_x_90_x_8    = AngleEqualLegSection(name="L 90 x 90 x 8"     , p_m=   10.9, p_h=     90, p_t=      8, p_r1=     11, p_r2=    5.5, p_A=  13.89, p_ys=   2.50, p_v=   6.36, p_u1=   3.53, p_u2=   3.17, p_AL=  0.351, p_AG=  32.15, p_Iy=  104.4, p_Wy=  16.05, p_iy=   2.74, p_Iu=  165.9, p_iu=   3.46, p_Iv=  42.87, p_iv=   1.76, p_Iyz= -61.51, p_phi=  24.00, p_emin=     48, p_emax=     51, p_Anet=  11.81)
    def L_90_x_90_x_9    = AngleEqualLegSection(name="L 90 x 90 x 9"     , p_m=   12.2, p_h=     90, p_t=      9, p_r1=     11, p_r2=    5.5, p_A=  15.52, p_ys=   2.54, p_v=   6.36, p_u1=   3.59, p_u2=   3.18, p_AL=  0.351, p_AG=  28.77, p_Iy=  115.8, p_Wy=  17.93, p_iy=   2.73, p_Iu=  184.0, p_iu=   3.44, p_Iv=  47.63, p_iv=   1.75, p_Iyz= -68.20, p_phi=  24.00, p_emin=     49, p_emax=     51, p_Anet=  13.18)
    def L_100_x_100_x_8  = AngleEqualLegSection(name="L 100 x 100 x 8"   , p_m=   12.2, p_h=    100, p_t=      8, p_r1=     12, p_r2=      6, p_A=  15.51, p_ys=   2.74, p_v=   7.07, p_u1=   3.87, p_u2=   3.52, p_AL=  0.390, p_AG=  32.00, p_Iy=  144.8, p_Wy=  19.94, p_iy=   3.06, p_Iu=  230.2, p_iu=   3.85, p_Iv=  59.47, p_iv=   1.96, p_Iyz= -85.37, p_phi=  27.00, p_emin=     53, p_emax=     55, p_Anet=  13.11)
    def L_100_x_100_x_10 = AngleEqualLegSection(name="L 100 x 100 x 10"  , p_m=   15.0, p_h=    100, p_t=     10, p_r1=     12, p_r2=      6, p_A=  19.15, p_ys=   2.82, p_v=   7.07, p_u1=   3.99, p_u2=   3.54, p_AL=  0.390, p_AG=  25.92, p_Iy=  176.7, p_Wy=  24.62, p_iy=   3.04, p_Iu=  280.7, p_iu=   3.83, p_Iv=  72.65, p_iv=   1.95, p_Iyz= -104.0, p_phi=  24.00, p_emin=     50, p_emax=     61, p_Anet=  16.55)
    def L_100_x_100_x_12 = AngleEqualLegSection(name="L 100 x 100 x 12"  , p_m=   17.8, p_h=    100, p_t=     12, p_r1=     12, p_r2=      6, p_A=  22.71, p_ys=   2.90, p_v=   7.07, p_u1=   4.11, p_u2=   3.57, p_AL=  0.390, p_AG=  21.86, p_Iy=  206.7, p_Wy=  29.12, p_iy=   3.02, p_Iu=  328.0, p_iu=   3.80, p_Iv=  85.42, p_iv=   1.94, p_Iyz= -121.3, p_phi=  24.00, p_emin=     52, p_emax=     61, p_Anet=  19.59)
    def L_110_x_110_x_10 = AngleEqualLegSection(name="L 110 x 110 x 10"  , p_m=   16.6, p_h=    110, p_t=     10, p_r1=     13, p_r2=    6.5, p_A=  21.18, p_ys=   3.06, p_v=   7.78, p_u1=   4.33, p_u2=   3.88, p_AL=  0.429, p_AG=  25.79, p_Iy=  238.0, p_Wy=  29.99, p_iy=   3.35, p_Iu=  378.2, p_iu=   4.23, p_Iv=  97.72, p_iv=   2.15, p_Iyz= -140.3, p_phi=  27.00, p_emin=     55, p_emax=     65, p_Anet=  18.18)
    def L_110_x_110_x_12 = AngleEqualLegSection(name="L 110 x 110 x 12"  , p_m=   19.7, p_h=    110, p_t=     12, p_r1=     13, p_r2=    6.5, p_A=  25.14, p_ys=   3.15, p_v=   7.78, p_u1=   4.45, p_u2=   3.91, p_AL=  0.429, p_AG=  21.73, p_Iy=  279.1, p_Wy=  35.54, p_iy=   3.33, p_Iu=  443.3, p_iu=   4.20, p_Iv=  115.0, p_iv=   2.14, p_Iyz= -164.1, p_phi=  27.00, p_emin=     57, p_emax=     65, p_Anet=  21.54)
    def L_120_x_120_x_10 = AngleEqualLegSection(name="L 120 x 120 x 10"  , p_m=   18.2, p_h=    120, p_t=     10, p_r1=     13, p_r2=    6.5, p_A=  23.18, p_ys=   3.31, p_v=   8.49, p_u1=   4.69, p_u2=   4.24, p_AL=  0.469, p_AG=  25.76, p_Iy=  312.9, p_Wy=  36.03, p_iy=   3.67, p_Iu=  497.6, p_iu=   4.63, p_Iv=  128.3, p_iv=   2.35, p_Iyz= -184.6, p_phi=  27.00, p_emin=     55, p_emax=     75, p_Anet=  20.18)
    def L_120_x_120_x_11 = AngleEqualLegSection(name="L 120 x 120 x 11"  , p_m=   19.9, p_h=    120, p_t=     11, p_r1=     13, p_r2=    6.5, p_A=  25.37, p_ys=   3.36, p_v=   8.49, p_u1=   4.75, p_u2=   4.25, p_AL=  0.469, p_AG=  23.54, p_Iy=  340.6, p_Wy=  39.41, p_iy=   3.66, p_Iu=  541.5, p_iu=   4.62, p_Iv=  139.8, p_iv=   2.35, p_Iyz= -200.9, p_phi=  27.00, p_emin=     56, p_emax=     75, p_Anet=  22.07)
    def L_120_x_120_x_12 = AngleEqualLegSection(name="L 120 x 120 x 12"  , p_m=   21.6, p_h=    120, p_t=     12, p_r1=     13, p_r2=    6.5, p_A=  27.54, p_ys=   3.40, p_v=   8.49, p_u1=   4.80, p_u2=   4.26, p_AL=  0.469, p_AG=  21.69, p_Iy=  367.7, p_Wy=  42.73, p_iy=   3.65, p_Iu=  584.3, p_iu=   4.61, p_Iv=  151.0, p_iv=   2.34, p_Iyz= -216.6, p_phi=  27.00, p_emin=     57, p_emax=     75, p_Anet=  23.94)
    def L_120_x_120_x_13 = AngleEqualLegSection(name="L 120 x 120 x 13"  , p_m=   23.3, p_h=    120, p_t=     13, p_r1=     13, p_r2=    6.5, p_A=  29.69, p_ys=   3.44, p_v=   8.49, p_u1=   4.86, p_u2=   4.28, p_AL=  0.469, p_AG=  20.12, p_Iy=  394.0, p_Wy=  46.01, p_iy=   3.64, p_Iu=  625.9, p_iu=   4.59, p_Iv=  162.2, p_iv=   2.34, p_Iyz= -231.8, p_phi=  27.00, p_emin=     58, p_emax=     75, p_Anet=  25.79)
    def L_120_x_120_x_15 = AngleEqualLegSection(name="L 120 x 120 x 15"  , p_m=   26.6, p_h=    120, p_t=     15, p_r1=     13, p_r2=    6.5, p_A=  33.93, p_ys=   3.51, p_v=   8.49, p_u1=   4.97, p_u2=   4.31, p_AL=  0.469, p_AG=  17.60, p_Iy=  444.9, p_Wy=  52.43, p_iy=   3.62, p_Iu=  705.6, p_iu=   4.56, p_Iv=  184.2, p_iv=   2.33, p_Iyz= -260.7, p_phi=  27.00, p_emin=     60, p_emax=     75, p_Anet=  29.43)
    def L_130_x_130_x_12 = AngleEqualLegSection(name="L 130 x 130 x 12"  , p_m=   23.5, p_h=    130, p_t=     12, p_r1=     14, p_r2=      7, p_A=  29.97, p_ys=   3.64, p_v=   9.19, p_u1=   5.15, p_u2=   4.60, p_AL=  0.508, p_AG=  21.59, p_Iy=  472.2, p_Wy=  50.44, p_iy=   3.97, p_Iu=  750.6, p_iu=   5.00, p_Iv=  193.7, p_iv=   2.54, p_Iyz= -278.5, p_phi=  27.00, p_emin=     57, p_emax=     85, p_Anet=  26.37)
    def L_140_x_140_x_10 = AngleEqualLegSection(name="L 140 x 140 x 10"  , p_m=   21.4, p_h=    140, p_t=     10, p_r1=     15, p_r2=    7.5, p_A=  27.24, p_ys=   3.79, p_v=   9.90, p_u1=   5.37, p_u2=   4.93, p_AL=  0.547, p_AG=  25.59, p_Iy=  504.4, p_Wy=  49.43, p_iy=   4.30, p_Iu=    802, p_iu=   5.43, p_Iv=  206.8, p_iv=   2.76, p_Iyz= -297.6, p_phi=  27.00, p_emin=     55, p_emax=     95, p_Anet=  24.24)
    def L_140_x_140_x_13 = AngleEqualLegSection(name="L 140 x 140 x 13"  , p_m=   27.4, p_h=    140, p_t=     13, p_r1=     15, p_r2=    7.5, p_A=  34.95, p_ys=   3.92, p_v=   9.90, p_u1=   5.55, p_u2=   4.96, p_AL=  0.547, p_AG=  19.94, p_Iy=  638.5, p_Wy=  63.37, p_iy=   4.27, p_Iu=   1015, p_iu=   5.39, p_Iv=  262.0, p_iv=   2.74, p_Iyz= -376.6, p_phi=  27.00, p_emin=     58, p_emax=     95, p_Anet=  31.05)
    def L_150_x_150_x_10 = AngleEqualLegSection(name="L 150 x 150 x 10"  , p_m=   23.0, p_h=    150, p_t=     10, p_r1=     16, p_r2=      8, p_A=  29.27, p_ys=   4.03, p_v=  10.61, p_u1=   5.71, p_u2=   5.28, p_AL=  0.586, p_AG=  25.51, p_Iy=  624.0, p_Wy=  56.91, p_iy=   4.62, p_Iu=    992, p_iu=   5.82, p_Iv=  256.0, p_iv=   2.96, p_Iyz= -368.0, p_phi=  27.00, p_emin=     55, p_emax=    105, p_Anet=  26.27)
    def L_150_x_150_x_12 = AngleEqualLegSection(name="L 150 x 150 x 12"  , p_m=   27.3, p_h=    150, p_t=     12, p_r1=     16, p_r2=      8, p_A=  34.83, p_ys=   4.12, p_v=  10.61, p_u1=   5.83, p_u2=   5.29, p_AL=  0.586, p_AG=  21.44, p_Iy=  736.9, p_Wy=  67.75, p_iy=   4.60, p_Iu=   1172, p_iu=   5.80, p_Iv=  302.0, p_iv=   2.94, p_Iyz= -434.9, p_phi=  27.00, p_emin=     57, p_emax=    105, p_Anet=  31.23)
    def L_150_x_150_x_14 = AngleEqualLegSection(name="L 150 x 150 x 14"  , p_m=   31.6, p_h=    150, p_t=     14, p_r1=     16, p_r2=      8, p_A=  40.31, p_ys=   4.21, p_v=  10.61, p_u1=   5.95, p_u2=   5.32, p_AL=  0.586, p_AG=  18.53, p_Iy=  845.4, p_Wy=  78.33, p_iy=   4.58, p_Iu=   1344, p_iu=   5.77, p_Iv=  346.9, p_iv=   2.93, p_Iyz= -498.5, p_phi=  27.00, p_emin=     59, p_emax=    105, p_Anet=  36.11)
    def L_150_x_150_x_15 = AngleEqualLegSection(name="L 150 x 150 x 15"  , p_m=   33.8, p_h=    150, p_t=     15, p_r1=     16, p_r2=      8, p_A=  43.02, p_ys=   4.25, p_v=  10.61, p_u1=   6.01, p_u2=   5.33, p_AL=  0.586, p_AG=  17.36, p_Iy=  898.1, p_Wy=  83.52, p_iy=   4.57, p_Iu=   1427, p_iu=   5.76, p_Iv=  368.9, p_iv=   2.93, p_Iyz= -529.1, p_phi=  27.00, p_emin=     60, p_emax=    105, p_Anet=  38.52)
    def L_150_x_150_x_18 = AngleEqualLegSection(name="L 150 x 150 x 18"  , p_m=   40.1, p_h=    150, p_t=     18, p_r1=     16, p_r2=      8, p_A=  51.03, p_ys=   4.37, p_v=  10.61, p_u1=   6.17, p_u2=   5.37, p_AL=  0.586, p_AG=  14.63, p_Iy=   1050, p_Wy=  98.74, p_iy=   4.54, p_Iu=   1666, p_iu=   5.71, p_Iv=  433.8, p_iv=   2.92, p_Iyz= -616.2, p_phi=  27.00, p_emin=     63, p_emax=    105, p_Anet=  45.63)
    def L_160_x_160_x_15 = AngleEqualLegSection(name="L 160 x 160 x 15"  , p_m=   36.2, p_h=    160, p_t=     15, p_r1=     17, p_r2=    8.5, p_A=  46.06, p_ys=   4.49, p_v=  11.31, p_u1=   6.35, p_u2=   5.67, p_AL=  0.625, p_AG=  17.30, p_Iy=   1099, p_Wy=  95.47, p_iy=   4.88, p_Iu=   1747, p_iu=   6.16, p_Iv=  450.8, p_iv=   3.13, p_Iyz= -648.0, p_phi=  27.00, p_emin=     60, p_emax=    115, p_Anet=  41.56)
    def L_160_x_160_x_17 = AngleEqualLegSection(name="L 160 x 160 x 17"  , p_m=   40.7, p_h=    160, p_t=     17, p_r1=     17, p_r2=    8.5, p_A=  51.82, p_ys=   4.57, p_v=  11.31, p_u1=   6.46, p_u2=   5.70, p_AL=  0.625, p_AG=  15.37, p_Iy=   1225, p_Wy=  107.2, p_iy=   4.86, p_Iu=   1947, p_iu=   6.13, p_Iv=  504.1, p_iv=   3.12, p_Iyz= -721.3, p_phi=  27.00, p_emin=     62, p_emax=    115, p_Anet=  46.72)
    def L_180_x_180_x_16 = AngleEqualLegSection(name="L 180 x 180 x 16"  , p_m=   43.5, p_h=    180, p_t=     16, p_r1=     18, p_r2=      9, p_A=  55.39, p_ys=   5.02, p_v=  12.73, p_u1=   7.10, p_u2=   6.38, p_AL=  0.705, p_AG=  16.20, p_Iy=   1682, p_Wy=  129.7, p_iy=   5.51, p_Iu=   2675, p_iu=   6.95, p_Iv=  689.4, p_iv=   3.53, p_Iyz= -993.1, p_phi=  27.00, p_emin=     61, p_emax=    135, p_Anet=  50.59)
    def L_180_x_180_x_18 = AngleEqualLegSection(name="L 180 x 180 x 18"  , p_m=   48.6, p_h=    180, p_t=     18, p_r1=     18, p_r2=      9, p_A=  61.91, p_ys=   5.10, p_v=  12.73, p_u1=   7.22, p_u2=   6.41, p_AL=  0.705, p_AG=  14.50, p_Iy=   1866, p_Wy=  144.7, p_iy=   5.49, p_Iu=   2965, p_iu=   6.92, p_Iv=  766.0, p_iv=   3.52, p_Iyz=  -1100, p_phi=  27.00, p_emin=     63, p_emax=    135, p_Anet=  56.51)
    def L_200_x_200_x_16 = AngleEqualLegSection(name="L 200 x 200 x 16"  , p_m=   48.5, p_h=    200, p_t=     16, p_r1=     18, p_r2=      9, p_A=  61.79, p_ys=   5.52, p_v=  14.14, p_u1=   7.81, p_u2=   7.09, p_AL=  0.785, p_AG=  16.18, p_Iy=   2341, p_Wy=  161.7, p_iy=   6.16, p_Iu=   3726, p_iu=   7.77, p_Iv=  957.2, p_iv=   3.94, p_Iyz=  -1384, p_phi=  27.00, p_emin=     61, p_emax=    155, p_Anet=  56.99)
    def L_200_x_200_x_18 = AngleEqualLegSection(name="L 200 x 200 x 18"  , p_m=   54.2, p_h=    200, p_t=     18, p_r1=     18, p_r2=      9, p_A=  69.11, p_ys=   5.60, p_v=  14.14, p_u1=   7.93, p_u2=   7.12, p_AL=  0.785, p_AG=  14.46, p_Iy=   2600, p_Wy=  180.6, p_iy=   6.13, p_Iu=   4135, p_iu=   7.74, p_Iv=   1064, p_iv=   3.92, p_Iyz=  -1536, p_phi=  27.00, p_emin=     63, p_emax=    155, p_Anet=  63.71)
    def L_200_x_200_x_20 = AngleEqualLegSection(name="L 200 x 200 x 20"  , p_m=   59.9, p_h=    200, p_t=     20, p_r1=     18, p_r2=      9, p_A=  76.35, p_ys=   5.68, p_v=  14.14, p_u1=   8.04, p_u2=   7.15, p_AL=  0.785, p_AG=  13.09, p_Iy=   2851, p_Wy=  199.1, p_iy=   6.11, p_Iu=   4532, p_iu=   7.70, p_Iv=   1169, p_iv=   3.91, p_Iyz=  -1681, p_phi=  27.00, p_emin=     65, p_emax=    155, p_Anet=  70.35)
    def L_200_x_200_x_24 = AngleEqualLegSection(name="L 200 x 200 x 24"  , p_m=   71.1, p_h=    200, p_t=     24, p_r1=     18, p_r2=      9, p_A=  90.59, p_ys=   5.84, p_v=  14.14, p_u1=   8.26, p_u2=   7.21, p_AL=  0.785, p_AG=  11.03, p_Iy=   3331, p_Wy=  235.2, p_iy=   6.06, p_Iu=   5286, p_iu=   7.64, p_Iv=   1375, p_iv=   3.90, p_Iyz=  -1955, p_phi=  27.00, p_emin=     69, p_emax=    155, p_Anet=  83.39)

}

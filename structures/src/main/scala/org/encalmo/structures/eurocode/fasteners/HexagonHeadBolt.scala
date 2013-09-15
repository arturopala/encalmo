package org.encalmo.structures.eurocode.fasteners

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

/** HexagonHeadBolt symbols */
trait HexagonHeadBoltSymbols extends SymbolConfigurator {

	val ID = symbol("ID").makeNonPrintable
    val CLASS = symbol("CLASS").makeNonPrintable
	val d = symbol(BasicSymbols.d) unit "mm"  //Średnica nominalna trzpienia
    val d2 = symbol(BasicSymbols.d|"2") unit "mm"  //Średnica zewnętrzna podkładki
    val s = symbol(BasicSymbols.s) unit "mm"  //Średnica wewnętrzna łba
    val e = symbol(BasicSymbols.e) unit "mm"  //Średnica zewnętrzna łba
    val k = symbol(BasicSymbols.k) unit "mm"  //Grubość łba
	val L = symbol(BasicSymbols.L) unit "mm"  //Długość trzpienia śruby
    val b = symbol(BasicSymbols.b) unit "mm"  //Długość części gwintowanej trzpienia
    val b1 = symbol(BasicSymbols.b|"L<125") unit "mm"  //Długość części gwintowanej trzpienia dla L<125mm
    val b2 = symbol(BasicSymbols.b|"L>125") unit "mm"  //Długość części gwintowanej trzpienia dla L>125mm
    val b3 = symbol(BasicSymbols.b|"L>200") unit "mm"  //Długość części gwintowanej trzpienia dla L>200mm
    val bv = symbol(BasicSymbols.b|BasicSymbols.v) unit "mm"  //Długość pełnej części trzpienia
    val m = symbol(BasicSymbols.m) unit "mm"  //Grubość nakrętki
    val w = symbol(BasicSymbols.w) unit "mm"  //Grubość podkładki
}

/** HexagonHeadBolt class */
class HexagonHeadBolt(name: String, val boltClass: BoltClass, p_d:Int, p_L:Expression, p_b1:Int, p_b2:Int, p_b3:Int, p_e:Double, p_k:Double, p_s:Double, p_m: Double, p_w: Double, p_d2: Double) extends Calculation(name,"hexagonheadbolt") with HexagonHeadBoltSymbols {

    this add boltClass

    import boltClass.{fyb,fub}

	ID := text(name)
    d := p_d
    s := p_s
    e := p_e
    k := p_k
    L := p_L
    b1 := p_b1
    b2 := p_b2
    b3 := p_b3
    b := (p_L match {case n:Number => if(n<=Number(125,SI.mm)) b1 else if(n<=Number(200,SI.mm)) b2 else b3; case _ => rangeChoiceLELE(L,b1,125,b2,200,b3)})
    bv := L-b
    m := p_m
    w := p_w
    d2 := p_d2

	def info = NumSection(Text(name),Text(boltClass.name),Text("Hexagon Head Bolt",dictionary),
		Evaluate(d,s,e,k,m,w,L,b,bv,fyb,fub)
	)

}

/** HexagonHeadBolt library */
object HexagonHeadBolt extends Catalog[HexagonHeadBolt]("HexagonHeadBolt") {

	override val map = Map[String,()=>HexagonHeadBolt](
		"M4 25 10.9" -> {() => M4(BoltClass.C_10_9,25)}
	)
	
	def  M4(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M4", clazz,4, p_L, 14,  0,  0,  7.66, 2.8,   7, 3.2, 0.8, 9)
    def  M5(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M5", clazz,5, p_L, 16,  0,  0,  8.79, 3.5,   8,  4, 1.0, 10)
    def  M6(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M6", clazz,6, p_L, 18, 24,  0, 11.05, 4.0,  10,  5, 1.6, 12)
    def  M8(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M8", clazz,8, p_L, 22, 28,  0, 14.38, 5.3,  13, 6.5, 1.6, 16)
    def M10(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M10",clazz,10,p_L, 26, 32, 45, 18.90, 6.4,  17,  8, 2.0, 20)
    def M12(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M12",clazz,12,p_L, 30, 36, 49, 21.10, 7.5,  19, 10, 2.5, 24)
    def M14(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M14",clazz,14,p_L, 34, 40, 53, 24.49, 8.8,  22, 11, 2.5, 28)
    def M16(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M16",clazz,16,p_L, 38, 44, 57, 26.75, 10.0, 24, 13, 3.0, 30)
    def M18(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M18",clazz,18,p_L, 42, 48, 61, 30.14, 11.5, 27, 15, 3.0, 34)
    def M20(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M20",clazz,20,p_L, 46, 52, 65, 33.53, 12.5, 30, 16, 3.0, 37)
    def M22(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M22",clazz,22,p_L, 50, 56, 69, 35.72, 14.0, 32, 18, 3.0, 39)
    def M24(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M24",clazz,24,p_L, 54, 60, 73, 39.98, 15.0, 36, 19, 4.0, 44)
    def M27(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M27",clazz,27,p_L, 60, 66, 79, 45.20, 17.0, 41, 22, 4.0, 50)
    def M30(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M30",clazz,30,p_L, 66, 72, 85, 50.85, 18.7, 46, 24, 4.0, 56)
    def M33(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M33",clazz,33,p_L, 72, 78, 91, 55.37, 21.0, 50, 26, 5.0, 60)
    def M36(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M36",clazz,36,p_L, 78, 84, 97, 60.79, 22.5, 55, 29, 5.0, 66)
    def M39(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M39",clazz,39,p_L, 84, 90,103, 66.44, 25.0, 60, 31, 6.0, 72)
    def M42(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M42",clazz,42,p_L, 90, 96,109, 71.30, 26.0, 65, 34, 6.0, 78)
    def M45(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M45",clazz,45,p_L, 96,102,115, 76.95, 28.0, 70, 36, 7.0, 85)
    def M48(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M48",clazz,48,p_L,102,108,121, 82.60, 30.0, 75, 38, 8.0, 92)
    def M52(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M52",clazz,52,p_L,  0,116,129, 88.60, 33.0, 80, 42, 8.0, 98)
    def M56(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M56",clazz,56,p_L,  0,  0,137, 93.56, 35.0, 85, 45, 9.0, 105)
    def M60(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M60",clazz,60,p_L,  0,  0,145, 98.30, 38.0, 90, 48, 9.0, 110)
    def M64(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M64",clazz,64,p_L,  0,  0,153,104.86, 40.0, 95, 51, 9.0, 115)
}
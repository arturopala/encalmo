package org.encalmo.structures.eurocode.fasteners

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

/** Bolt symbols */
trait BoltSymbols extends SymbolConfigurator {

    val boltDict = "bolt"

    val ID = symbol("ID").makeNonPrintable
    val d = symbol(BasicSymbols.d) unit "mm" dict boltDict  //Średnica nominalna trzpienia
    val d0 = symbol(BasicSymbols.d|0) unit "mm" dict boltDict  //Średnica otworu przejściowego
    val Delta = symbol(BasicSymbols.Delta) unit "mm" dict boltDict  //Luz otworu przejściowego
    val L = symbol(BasicSymbols.L) unit "mm" dict boltDict  //Długość trzpienia śruby
    val b = symbol(BasicSymbols.b) unit "mm" dict boltDict  //Długość części gwintowanej trzpienia
    val bv = symbol(BasicSymbols.b|BasicSymbols.v) unit "mm" dict boltDict  //Długość pełnej części trzpienia
    val d2 = symbol(BasicSymbols.d|2) unit "mm" dict boltDict  //Średnica zewnętrzna podkładki
    val dm = symbol(BasicSymbols.d|BasicSymbols.m) unit "mm" dict boltDict  //Średnia średnic łba
    val s = symbol(BasicSymbols.s) unit "mm" dict boltDict  //Średnica wewnętrzna łba
    val e = symbol(BasicSymbols.e) unit "mm" dict boltDict  //Średnica zewnętrzna łba
    val k = symbol(BasicSymbols.k) unit "mm" dict boltDict  //Grubość łba
    val m = symbol(BasicSymbols.m) unit "mm" dict boltDict  //Grubość nakrętki
    val w = symbol(BasicSymbols.w) unit "mm" dict boltDict  //Grubość podkładki

    val A = symbol(BasicSymbols.A) unit "mm2" dict boltDict  //Powierzchnia przekroju
    val As = symbol(BasicSymbols.A|BasicSymbols.s) unit "mm2" dict boltDict  //Powierzchnia przekroju części gwintowanej
    val t = symbol(BasicSymbols.t) unit "mm" dict boltDict  //Grubość cieńszej części łączonej
    val fu = symbol(BasicSymbols.f|"u") unit SI.MPa dict boltDict //Wytrzymałość na rozciąganie stali części łączonej

    val e1 = symbol(BasicSymbols.e|1) unit "mm" dict boltDict  //Minimalna odległość czołowa od osi otworu
    val e2 = symbol(BasicSymbols.e|2) unit "mm" dict boltDict  //Minimalna odległość boczna od osi otworu
    val e3 = symbol(BasicSymbols.e|3) unit "mm" dict boltDict  //Minimalna odległość czołowa od osi otworu owalnego
    val e4 = symbol(BasicSymbols.e|4) unit "mm" dict boltDict  //Minimalna odległość boczna od osi otworu owalnego
    val e1max = symbol(BasicSymbols.e|"1,max") unit "mm" dict boltDict  //Maksymalna odległość czołowa od osi otworu
    val e2max = symbol(BasicSymbols.e|"2,max") unit "mm" dict boltDict  //Maksymalna odległość boczna od osi otworu
    val p1 = symbol(BasicSymbols.p|1) unit "mm" dict boltDict  //Minimalny rozstaw podłużny osi otworów w elementach ściskanych
    val p2 = symbol(BasicSymbols.p|2) unit "mm" dict boltDict  //Minimalny rozstaw poprzeczny osi otworów
    val p1max = symbol(BasicSymbols.p|"1,max") unit "mm" dict boltDict  //Maksymalny rozstaw podłużny osi otworów w elementach ściskanych
    val p2max = symbol(BasicSymbols.p|"2,max") unit "mm" dict boltDict  //Maksymalny rozstaw poprzeczny osi otworów
    val p10max = symbol(BasicSymbols.p|"1,0,max") unit "mm" dict boltDict  //Maksymalny rozstaw podłużny zewnętrznego szeregu osi otworów w elementach rozciąganych
    val p1imax = symbol(BasicSymbols.p|"1,i,max") unit "mm" dict boltDict  //Maksymalny rozstaw podłużny wewnętrznego szeregu osi otworów w elementach rozciąganych

    val gammaM2 = symbol(BasicSymbols.gamma|"M2") dict boltDict //Współczynnik częściowy przy obliczaniu nośności śrub, nitów, sworzni
    val gammaM3 = symbol(BasicSymbols.gamma|"M3") dict boltDict //Współczynnik częściowy przy obliczaniu nośności na poślizg
    val gammaM3ser = symbol(BasicSymbols.gamma|"M3,ser") dict boltDict //Współczynnik częściowy przy obliczaniu nośności na poślizg
    val gammaM7 = symbol(BasicSymbols.gamma|"M7") dict boltDict //Współczynnik częściowy przy obliczaniu siły sprężania śrub wysokiej wytrzymałości
    val alphab0 = symbol(BasicSymbols.alpha|"b,0") dict boltDict //Współczynnik nośności na docisk śruby skrajnej
    val alphabi = symbol(BasicSymbols.alpha|"b,i") dict boltDict //Współczynnik nośności na docisk śruby posredniej
    val alphab = symbol(BasicSymbols.alpha|"b") dict boltDict //Współczynnik nośności na docisk
    val k10 = symbol(BasicSymbols.k|"1,0") dict boltDict //Współczynnik pomocniczy nośności na docisk śruby skrajnej
    val k1i = symbol(BasicSymbols.k|"1,i") dict boltDict //Współczynnik pomocniczy nośności na docisk śruby skrajnej
    val k1 = symbol(BasicSymbols.k|"1") dict boltDict //Współczynnik pomocniczy nośności na docisk
    val ks = symbol(BasicSymbols.k|"1") dict boltDict //Współczynnik kształtu otworów na śruby
    val mu = symbol(BasicSymbols.mu) dict boltDict //Współczynnik tarcia elementów łączonych (według PN-EN 1090-2)
    val FpC = symbol(BasicSymbols.F|"p,C") unit SI.kN dict boltDict //Charakterystyczna siła sprężenia
    val FpCd = symbol(BasicSymbols.F|"p,Cd") unit SI.kN dict boltDict //Obliczeniowa siła sprężenia
    val FvRd = symbol(BasicSymbols.F|"v,Rd") unit SI.kN dict boltDict //Nośność na ścinanie w jednej płaszczyźnie
    val FvsRd = symbol(BasicSymbols.F|"vs,Rd") unit SI.kN dict boltDict //Nośność na ścinanie przez gwint w jednej płaszczyźnie
    val FbRd = symbol(BasicSymbols.F|"b,Rd") unit SI.kN dict boltDict //Nośność na docisk
    val FtRd = symbol(BasicSymbols.F|"t,Rd") unit SI.kN dict boltDict //Nośność na rozciąganie
    val BpRd = symbol(BasicSymbols.B|"p,Rd") unit SI.kN dict boltDict //Nośność na przeciąganie
    val FsRdB = symbol(BasicSymbols.F|"s,Rd,B") unit SI.kN dict boltDict //Nośność na poślizg w połączeniu sprężonym kategorii B
    val FsRdC = symbol(BasicSymbols.F|"s,Rd,C") unit SI.kN dict boltDict //Nośność na poślizg w połączeniu sprężonym kategorii C
}

/** HexagonHeadBolt symbols */
trait HexagonHeadBoltSymbols extends BoltSymbols {

    val b1 = symbol(BasicSymbols.b|"L<125") unit "mm"  //Długość części gwintowanej trzpienia dla L<125mm
    val b2 = symbol(BasicSymbols.b|"L>125") unit "mm"  //Długość części gwintowanej trzpienia dla L>125mm
    val b3 = symbol(BasicSymbols.b|"L>200") unit "mm"  //Długość części gwintowanej trzpienia dla L>200mm

}

/** HexagonHeadBolt class */
class HexagonHeadBolt(name: String, val boltClass: BoltClass, p_d:Int, p_L:Expression, p_b1:Int, p_b2:Int, p_b3:Int, p_e:Double, p_k:Double, p_s:Double, p_m: Double, p_w: Double, p_d2: Double, p_Delta: Double) extends Calculation(name,"hexagonheadbolt") with HexagonHeadBoltSymbols {

    this add boltClass

    import boltClass.{fyb,fub,alphav}

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
    Delta := p_Delta
    d0 := d+Delta
    dm := floor((s+e)/2)
    A := (PI*(d^2))/4
    As := 0.78*A
    t := Number(10,SI.mm)
    fu := 510 unit SI.MPa

    e1 := 1.2*d0
    e2 := 1.2*d0
    e3 := 1.5*d0
    e4 := 1.5*d0
    p1 := 2.2*d0
    p2 := 2.4*d0

    e1max := 4*t+Number(40,SI.mm)
    e2max := 4*t+Number(40,SI.mm)
    p1max := min(14*t,Number(200,SI.mm))
    p2max := min(14*t,Number(200,SI.mm))
    p10max := min(14*t,Number(200,SI.mm))
    p1imax := min(28*t,Number(200,SI.mm))

    gammaM2 := 1.25
    gammaM3 := 1.25
    gammaM3ser := 1.1
    gammaM7 := 1.1
    FpC := 0.7*fub*As
    FpCd := (0.7*fub*As)/gammaM7
    FvsRd := (alphav*fub*As)/gammaM2
    FvRd := (0.6*fub*A)/gammaM2
    k10 := min(2.8 * (e2 / d0) - 1.7,2.5)
    k1i := min(1.4 * (p2 / d0) - 1.7,2.5)
    k1 := min(k10,k1i)
    alphab0 := min(e1/(3*d0),fub/fu,1.0)
    alphabi := min(p1/(3*d0)-0.25,fub/fu,1.0)
    alphab := min(alphab0,alphabi)
    FbRd := (k1*alphab*fu*d*t)/gammaM2
    FtRd := (0.9*fub*As)/gammaM2
    BpRd := (0.6*PI*dm*t*fu)/gammaM2

    ks := 1.0
    mu := 0.5
    FsRdB := (ks*mu*FpC)/gammaM3ser
    FsRdC := (ks*mu*FpC)/gammaM3

	def info = NumSection(Text(name),Text(boltClass.name),Text("Hexagon Head Bolt",dictionary),
		Evaluate(d,d0,s,e,k,m,w,L,b,bv,t),
        AssertionRangeLELE("doboru średnicy śruby",1.5*t,d,2.5*t),
        "Odległości od krawędzi i rozstawy otworów",
        Evaluate(e1,e2,e3,e4,p1,p2),
        "Wytrzymałość i nośność",
        Evaluate(A,fyb,fub,gammaM2,FpCd,FvRd,FvsRd,fu,k10,k1i,k1,alphab0,alphabi,alphab,FbRd,FtRd,BpRd,FsRdC)
	)

}

/** HexagonHeadBolt library */
object HexagonHeadBolt {
	
	def  M4(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M4", clazz,4, p_L, 14,  0,  0,  7.66, 2.8,   7, 3.2, 0.8, 9,0.5)
    def  M5(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M5", clazz,5, p_L, 16,  0,  0,  8.79, 3.5,   8,  4, 1.0, 10,0.5)
    def  M6(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M6", clazz,6, p_L, 18, 24,  0, 11.05, 4.0,  10,  5, 1.6, 12,0.5)
    def  M8(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M8", clazz,8, p_L, 22, 28,  0, 14.38, 5.3,  13, 6.5, 1.6, 16,1)
    def M10(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M10",clazz,10,p_L, 26, 32, 45, 18.90, 6.4,  17,  8, 2.0, 20,1)
    def M12(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M12",clazz,12,p_L, 30, 36, 49, 21.10, 7.5,  19, 10, 2.5, 24,1)
    def M14(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M14",clazz,14,p_L, 34, 40, 53, 24.49, 8.8,  22, 11, 2.5, 28,2)
    def M16(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M16",clazz,16,p_L, 38, 44, 57, 26.75, 10.0, 24, 13, 3.0, 30,2)
    def M18(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M18",clazz,18,p_L, 42, 48, 61, 30.14, 11.5, 27, 15, 3.0, 34,2)
    def M20(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M20",clazz,20,p_L, 46, 52, 65, 33.53, 12.5, 30, 16, 3.0, 37,2)
    def M22(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M22",clazz,22,p_L, 50, 56, 69, 35.72, 14.0, 32, 18, 3.0, 39,2)
    def M24(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M24",clazz,24,p_L, 54, 60, 73, 39.98, 15.0, 36, 19, 4.0, 44,2)
    def M27(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M27",clazz,27,p_L, 60, 66, 79, 45.20, 17.0, 41, 22, 4.0, 50,3)
    def M30(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M30",clazz,30,p_L, 66, 72, 85, 50.85, 18.7, 46, 24, 4.0, 56,3)
    def M33(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M33",clazz,33,p_L, 72, 78, 91, 55.37, 21.0, 50, 26, 5.0, 60,3)
    def M36(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M36",clazz,36,p_L, 78, 84, 97, 60.79, 22.5, 55, 29, 5.0, 66,3)
    def M39(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M39",clazz,39,p_L, 84, 90,103, 66.44, 25.0, 60, 31, 6.0, 72,3)
    def M42(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M42",clazz,42,p_L, 90, 96,109, 71.30, 26.0, 65, 34, 6.0, 78,3)
    def M45(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M45",clazz,45,p_L, 96,102,115, 76.95, 28.0, 70, 36, 7.0, 85,3)
    def M48(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M48",clazz,48,p_L,102,108,121, 82.60, 30.0, 75, 38, 8.0, 92,4)
    def M52(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M52",clazz,52,p_L,  0,116,129, 88.60, 33.0, 80, 42, 8.0, 98,4)
    def M56(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M56",clazz,56,p_L,  0,  0,137, 93.56, 35.0, 85, 45, 9.0, 105,4)
    def M60(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M60",clazz,60,p_L,  0,  0,145, 98.30, 38.0, 90, 48, 9.0, 110,4)
    def M64(clazz:BoltClass, p_L: Expression) = new HexagonHeadBolt("M64",clazz,64,p_L,  0,  0,153,104.86, 40.0, 95, 51, 9.0, 115,4)
}
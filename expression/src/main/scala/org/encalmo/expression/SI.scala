package org.encalmo.expression

/**
 * International System_of Units 
 * [http://en.wikipedia.org/wiki/International_System_of_Units]
 * [http://en.wikipedia.org/wiki/SI_prefix]
 */
object SI extends UnitOfValueSystem {
    
    val meter:UnitOfValueName = UnitOfValueName("m")
    val newton:UnitOfValueName = UnitOfValueName("N")
    val pascal:UnitOfValueName = UnitOfValueName("Pa")

	override def apply(scale:Int):Option[UnitOfValueScale] = prefixMap.get(scale)

	object Prefix {
	
		val empty = UnitOfValueScale("",1)
		val da = UnitOfValueScale("da",1E1)
		val h = UnitOfValueScale("h",1E2)
		val k = UnitOfValueScale("k",1E3)
		val M = UnitOfValueScale("M",1E6)
		val G = UnitOfValueScale("G",1E9)
		val P = UnitOfValueScale("P",1E12)
		val E = UnitOfValueScale("E",1E15)
		val d = UnitOfValueScale("d",1E-1)
		val c = UnitOfValueScale("c",1E-2)
		val m = UnitOfValueScale("m",1E-3)
		val μ = UnitOfValueScale("μ",1E-6)
		val n = UnitOfValueScale("n",1E-9)
		val p = UnitOfValueScale("p",1E-12)
		val f = UnitOfValueScale("f",1E-15)
	}
	
	import Prefix._
	
	private val seq:Seq[Int] = Seq(-15,-12,-9,-6,-3,-2,-1,0,1,2,3,6,9,12,15)

	private val prefixMap:Map[Int,UnitOfValueScale] = Map(
		0 -> empty,
		1 -> Prefix.da, 2 -> Prefix.h, 3 -> Prefix.k, 6 -> Prefix.M, 9 -> Prefix.G, 12 -> Prefix.P, 15 -> Prefix.E,
		-1 -> Prefix.d, -2 -> Prefix.c, -3 -> Prefix.m, -6 -> Prefix.μ, -9 -> Prefix.n, -12 -> Prefix.p, -15 -> Prefix.f
	)
	
	// length
	val m = BaseUnitOfValue(meter,0,1,this,Characteristics.Length)
	val dm = m exp -1
	val cm = m exp -2
	val mm = m exp -3
	val μm = m exp -6
	val nm = m exp -9
	val km = m exp 3
	// area
	val m2 = m dim 2 set Characteristics.Area
	val dm2 = m dim 2 exp -1
	val cm2 = m dim 2 exp -2
	val mm2 = m dim 2 exp -3
	val km2 = m dim 2 exp 3
	// volume
	val m3 = m dim 3 set Characteristics.Volume
	val dm3 = m dim 3 exp -1
	val cm3 = m dim 3 exp -2
	val mm3 = m dim 3 exp -3
	// others
	val m4 = m dim 4
	val m6 = m dim 6
	val m8 = m dim 8
	// force
	val N = BaseUnitOfValue(newton,0,1,this,Characteristics.Force)
	val kN = N exp 3
	val MN = N exp 6
	val GN = N exp 9
	// pressure
	val Pa = BaseUnitOfValue(pascal,0,1,this,Characteristics.Pressure)
	val kPa = Pa exp 3
    val MPa = Pa exp 6
    val GPa = Pa exp 9
    
    override def apply(name:String):Option[UnitOfValue] = unitMap.get(name)
    
    private lazy val unitMap:Map[String,UnitOfValue] = Map(
		m.name.toString -> m,
		dm.name.toString -> dm,
		cm.name.toString -> cm,
		mm.name.toString -> mm,
		μm.name.toString -> μm,
		nm.name.toString -> nm,
		km.name.toString -> km,
		N.name.toString -> N,
		kN.name.toString -> kN,
		MN.name.toString -> MN,
		GN.name.toString -> GN,
		Pa.name.toString -> Pa,
		kPa.name.toString -> kPa,
		MPa.name.toString -> MPa,
		GPa.name.toString -> GPa
	)
    
}
package org.encalmo.expression

/**
 * International System_of Units 
 * [http://en.wikipedia.org/wiki/International_System_of_Units]
 * [http://en.wikipedia.org/wiki/SI_prefix]
 */
object SI extends UnitOfValueSystem {
    
    override def toString:String = "SI"
    
    private var initialized = false

    val meter:UnitOfValueName = UnitOfValueName("m")
    val gram:UnitOfValueName = UnitOfValueName("g")
    val second:UnitOfValueName = UnitOfValueName("s")
    val minute:UnitOfValueName = UnitOfValueName("min")
    val hour:UnitOfValueName = UnitOfValueName("h")
    val newton:UnitOfValueName = UnitOfValueName("N")
    val newtonmeter:UnitOfValueName = UnitOfValueName("Nm")
    val pascal:UnitOfValueName = UnitOfValueName("Pa")
    val degree:UnitOfValueName = UnitOfValueName("°")
    val radian:UnitOfValueName = UnitOfValueName("rad")

	override def apply(scale:Int):Option[UnitOfValueScale] = prefixMap.get(scale)
	
	override def find(baseName:String,scale:Int,dimension:Double):Option[UnitOfValue] = if(initialized) units.find(
	        u => u.name.baseName == baseName && u.scale == scale && u.dimension == dimension) else None
	        
	override def expand(u:UnitOfValue):UnitOfValue = expansionMap.get(u).getOrElse(u)

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

	private val prefixMap:Map[Int,UnitOfValueScale] = Map(
		0 -> empty,
		1 -> Prefix.da, 2 -> Prefix.h, 3 -> Prefix.k, 6 -> Prefix.M, 9 -> Prefix.G, 12 -> Prefix.P, 15 -> Prefix.E,
		-1 -> Prefix.d, -2 -> Prefix.c, -3 -> Prefix.m, -6 -> Prefix.μ, -9 -> Prefix.n, -12 -> Prefix.p, -15 -> Prefix.f
	)
	
	val percent = SimpleUnitOfValue(EmptyUnitOfValueName,-2,1,this,Characteristics.None,Some(UnitOfValueName("%")))
	val permille = SimpleUnitOfValue(EmptyUnitOfValueName,-3,1,this,Characteristics.None,Some(UnitOfValueName("‰")))

	// length
	val m = SimpleUnitOfValue(meter,0,1,this,Characteristics.Length)
	val dm = m exp -1
	val cm = m exp -2
	val mm = m exp -3
	val μm = m exp -6
	val nm = m exp -9
	val km = m exp 3
	// area
	val m2 = m set Characteristics.Area dim 2
	val dm2 = m2 exp -1
	val cm2 = m2 exp -2
	val mm2 = m2 exp -3
	val km2 = m2 exp 3
	// volume
	val m3 = m set Characteristics.Volume dim 3
	val dm3 = m3 exp -1
	val cm3 = m3 exp -2
	val mm3 = m3 exp -3
	// others
	val m4 = m set Characteristics.None dim 4
	val cm4 = m4 exp -2
	val mm4 = m4 exp -3
	val m6 = m set Characteristics.None dim 6
	val cm6 = m6 exp -2
	val mm6 = m6 exp -3
	val m8 = m set Characteristics.None dim 8
	val cm8 = m8 exp -2
	val mm8 = m8 exp -3
	//weight
	val g = SimpleUnitOfValue(gram,0,1,this,Characteristics.Weight)
	val mg = g exp -3
	val kg = g exp 3
	val t = SimpleUnitOfValue(gram,6,1,this,Characteristics.Weight,Some(UnitOfValueName("t")))
	// force
	val N = SimpleUnitOfValue(newton,0,1,this,Characteristics.Force)
	val kN = N exp 3
	val MN = N exp 6
	val GN = N exp 9
	// pressure
	val Pa = SimpleUnitOfValue(pascal,0,1,this,Characteristics.Pressure)
	val kPa = Pa exp 3
    val MPa = Pa exp 6
    val GPa = Pa exp 9
    // moment of force
    val Nm = SimpleUnitOfValue(newtonmeter,0,1,this,Characteristics.MomentOfForce)
    val kNm = Nm exp 3
    val MNm = Nm exp 6
    //angle
    val deg = SimpleUnitOfValue(degree,0,1,this,Characteristics.Angle)
    val rad = SimpleUnitOfValue(radian,0,1,this,Characteristics.Angle)
    //time
    val s = SimpleUnitOfValue(second,0,1,this,Characteristics.Time)
    val ms = s exp -3
    val s2 = s dim 2
    val min = SimpleUnitOfValue(minute,0,1,this,Characteristics.Time)
    val h = SimpleUnitOfValue(hour,0,1,this,Characteristics.Time)
    // velocity
    val mps = m/s
    val mmpersec = mm/s
    val mmpermin = mm/min
    // acceleration
    val mps2 = m/s2
	
	override val units:Seq[UnitOfValue] = Seq(
	        // simple units
	        percent,permille,
	        m,dm,cm,mm,μm,nm,km,
	        g,kg,mg,t,
            s,ms,s2,min,h,
	        N,kN,MN,GN,
	        Pa,kPa,MPa,GPa,
	        m2,dm2,cm2,mm2,km2,
	        m3,dm3,cm3,mm3,
	        m4,cm4,mm4,
	        m6,cm6,mm6,
	        m8,cm8,mm8,
	        deg,rad,
	        Nm,kNm,MNm,
	        // complex derived units
	        mps,mmpersec,mmpermin,mps2,
	        (kg*m)/s2,kg/m,kg/m2,kg/m3,
	        N/m,N/mm,N/m2,N/mm2,N/m3,N*m,N*mm,N*m2,Nm/m,
	        kN/m,kN/m2,kN/m3,kNm/m,
	        MN/m,MN/m2,MN/m3,MNm/m,
	        m2/m,m3/m,m4/m,m6/m,m8/m,
	        cm2/m,cm3/m,cm4/m,
	        m3/min,m3/h
    )
    
    val expansionMap:Map[UnitOfValue,UnitOfValue] = Map(
            N -> kg*m/s2,
            Pa -> N/m2,
            kPa -> kN/m2,
            MPa -> MN/m2,
            GPa -> GN/m2,
            Nm -> N*m,
            kNm -> kN*m,
            MNm -> MN*m,
            min -> ComplexUnitOfValue(60*s),
            h -> ComplexUnitOfValue(60*min)
    )
	
	initialized = true
    
}
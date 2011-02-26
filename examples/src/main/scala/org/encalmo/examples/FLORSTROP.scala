package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext

object FLORSTROP {

	import ProfiledSteelSheetSymbols._

	def T59_Z_075 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.75", Steel.S280GD, data_T59_Z_075)
	private lazy val data_T59_Z_075 = new MapContext {
		this(hp) = 59E-3
		this(br) = 44.6E-3
		this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
		this(t) = 0.75E-3
		this(r) = 5E-3
		this(Iminus) = 34.87E-8
        this(Iplus) = 54.42E-8
        this(Ap) = 12.53E-4
        this(eminus) = 3.06E-2
        this(eplus) = 4.03E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
	}
	
	def T59_Z_088 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.88", Steel.S280GD, data_T59_Z_088)
	private lazy val data_T59_Z_088 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 0.88E-3
		this(r) = 5E-3
        this(Iminus) = 42.65E-8
        this(Iplus) = 63.66E-8
        this(Ap) = 15.04E-4
        this(eminus) = 3.16E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }
	
	def T59_Z_100 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.0", Steel.S280GD, data_T59_Z_100)
	private lazy val data_T59_Z_100 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.0E-3
		this(r) = 5E-3
        this(Iminus) = 49.61E-8
        this(Iplus) = 73.23E-8
        this(Ap) = 17.35E-4
        this(eminus) = 3.27E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }
	
    def T59_Z_125 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.25", Steel.S280GD, data_T59_Z_125)
	private lazy val data_T59_Z_125 = new MapContext {
        this(hp) = 59E-3
        this(br) = 44.6E-3
        this(bs) = 140E-3
		this(bo) = 95.4E-3
		this(bb) = 127E-3
        this(t) = 1.25E-3
		this(r) = 5E-3
        this(Iminus) = 65.84E-8
        this(Iplus) = 96.48E-8
        this(Ap) = 22.17E-4
        this(eminus) = 3.46E-2
        this(eplus) = 4.06E-2
        this(ep) = 3.64E-2
        this(Phi) = 75
        lock
    }

}
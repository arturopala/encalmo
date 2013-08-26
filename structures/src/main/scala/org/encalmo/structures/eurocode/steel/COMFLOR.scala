package org.encalmo.structures.eurocode.steel

import org.encalmo.calculation.MapContext

object COMFLOR {

	import ProfiledSteelSheetSymbols._

	def COMFLOR_46_90 = new ProfiledSteelSheet("COMFLOR 46 0.9", Steel.S280GD, data_46_90)
	
	private lazy val data_46_90 = new MapContext {
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
        lock()
	}

}

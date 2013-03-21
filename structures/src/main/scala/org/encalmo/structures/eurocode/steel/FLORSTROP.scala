package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.calculation.MapContext

object FLORSTROP {

	import ProfiledSteelSheetSymbols._

	def T59_Z_075 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.75", Steel.S280GD, data_T59_Z_075)
	private lazy val data_T59_Z_075 = new MapContext {
		hp := 59
		br := 44.6
		bs := 140
		bo := 95.4
		bb := 127
		t := 0.75
		r := 5
		Iminus := 34.87E-8
        Iplus := 54.42E-8
        Ap := 12.53E-4
        eminus := 30.6
        eplus := 40.3
        ep := 36.4
        Phi := 75
        lock
	}
	
	def T59_Z_088 = new ProfiledSteelSheet("FLORSTROP T59 Z 0.88", Steel.S280GD, data_T59_Z_088)
	private lazy val data_T59_Z_088 = new MapContext {
        hp := 59
        br := 44.6
        bs := 140
		bo := 95.4
		bb := 127
        t := 0.88
		r := 5
        Iminus := 42.65E-8
        Iplus := 63.66E-8
        Ap := 15.04E-4
        eminus := 31.6
        eplus := 40.6
        ep := 36.4
        Phi := 75
        lock
    }
	
	def T59_Z_100 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.0", Steel.S280GD, data_T59_Z_100)
	private lazy val data_T59_Z_100 = new MapContext {
        hp := 59
        br := 44.6
        bs := 140
		bo := 95.4
		bb := 127
        t := 1.0
		r := 5E-3
        Iminus := 49.61E-8
        Iplus := 73.23E-8
        Ap := 17.35E-4
        eminus := 32.7
        eplus := 40.6
        ep := 36.4
        Phi := 75
        lock
    }
	
    def T59_Z_125 = new ProfiledSteelSheet("FLORSTROP T59 Z 1.25", Steel.S280GD, data_T59_Z_125)
	private lazy val data_T59_Z_125 = new MapContext {
        hp := 59
        br := 44.6
        bs := 140
		bo := 95.4
		bb := 127
        t := 1.25
		r := 5
        Iminus := 65.84E-8
        Iplus := 96.48E-8
        Ap := 22.17E-4
        eminus := 34.6
        eplus := 40.6
        ep := 36.4
        Phi := 75
        lock
    }

}

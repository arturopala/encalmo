package org.encalmo.structures.common.statics

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.expression.abs

trait ContinuousBeamSymbols extends SymbolConfigurator {
	
	val p = symbol(BasicSymbols.p)
	val l = symbol(BasicSymbols.l)
	val P = symbol(BasicSymbols.P)
	
	val lBA = symbol(BasicSymbols.l|"BA")
	val lBC = symbol(BasicSymbols.l|"BC")
	val lCB = symbol(BasicSymbols.l|"CB")
	val lCD = symbol(BasicSymbols.l|"CD")
	
	val Mmax = symbol(BasicSymbols.M|"max")
	val Mmin = symbol(BasicSymbols.M|"min")
	val Rmax = symbol(BasicSymbols.R|"max")
	val TRmax1 = symbol(BasicSymbols.T|"Rmax,1")
	val TRmax2 = symbol(BasicSymbols.T|"Rmax,2")
	val Rmin = symbol(BasicSymbols.R|"min")
	val Tmax = symbol(BasicSymbols.T|"max")
	val Tmin = symbol(BasicSymbols.T|"min")
	
	
	val M11max = symbol(BasicSymbols.M|"11,max")
	val M12max = symbol(BasicSymbols.M|"12,max")
	val M21max = symbol(BasicSymbols.M|"21,max")
	val M22max = symbol(BasicSymbols.M|"22,max")
	val M31max = symbol(BasicSymbols.M|"31,max")
	val M32max = symbol(BasicSymbols.M|"32,max")
	val MBmax = symbol(BasicSymbols.M|"B,max")
	val MCmax = symbol(BasicSymbols.M|"C,max")
	val RAmax = symbol(BasicSymbols.R|"A,max")
	val RBmax = symbol(BasicSymbols.R|"B,max")
	val RCmax = symbol(BasicSymbols.R|"C,max")
	val T1Bmax = symbol(BasicSymbols.T|"1,B,max")
	val T2Bmax = symbol(BasicSymbols.T|"2,B,max")
	val T2Cmax = symbol(BasicSymbols.T|"2,C,max")
	val T3Cmax = symbol(BasicSymbols.T|"3,C,max")
	
	val M11min = symbol(BasicSymbols.M|"11,min")
	val M12min = symbol(BasicSymbols.M|"12,min")
	val M21min = symbol(BasicSymbols.M|"21,min")
	val M22min = symbol(BasicSymbols.M|"22,min")
	val M31min = symbol(BasicSymbols.M|"31,min")
	val M32min = symbol(BasicSymbols.M|"32,min")
	val MBmin = symbol(BasicSymbols.M|"B,min")
	val MCmin = symbol(BasicSymbols.M|"C,min")
	val RAmin = symbol(BasicSymbols.R|"A,min")
	val RBmin = symbol(BasicSymbols.R|"B,min")
	val RCmin = symbol(BasicSymbols.R|"C,min")
	val T1Bmin = symbol(BasicSymbols.T|"1,B,min")
	val T2Bmin = symbol(BasicSymbols.T|"2,B,min")
	val T2Cmin = symbol(BasicSymbols.T|"2,C,min")
	val T3Cmin = symbol(BasicSymbols.T|"3,C,min")

}

class ContinuousBeam(name:String, length:Expression, load:Expression, force:Expression) extends MapContext("beam") with ContinuousBeamSymbols {
	
	p := load
	l := length
	P := force

}

/** Continuous beam static analysis */
class ContinuousBeam_5_LinearLoad(id:String, length:Expression, load:Expression) extends ContinuousBeam(id,length,load,0) {
	
	lBA := 0.2113*l
	lBC := 0.2*l
	lCB := 0.2113*l
	lCD := 0.2105*l
	
	M11max := 0.1*p*(l^2)
	M12max := 0
	M21max := 0.079*p*(l^2)
	M22max := 0
	M31max := 0.086*p*(l^2)
	M32max := 0
	
	MBmax := 0.014*p*(l^2)
	MCmax := 0.032*p*(l^2)
	
	RAmax := 0.447*p*l
	RBmax := 1.218*p*l
	RCmax := 1.167*p*l
	
	T1Bmax := 0.014*p*l
	T2Bmax := 0.598*p*l
	T2Cmax := 0.103*p*l
	T3Cmax := 0.591*p*l
	
	M11min := 0
	M12min := 0
	M21min := 0
	M22min := 0
	M31min := 0
	M32min := 0
	
	MBmin := -0.120*p*(l^2)
	MCmin := -0.111*p*(l^2)
	
	RAmin := -0.053*p*l
	RBmin := -0.086*p*l
	RCmin := -0.194*p*l
	
	T1Bmin := -0.62*p*l
	T2Bmin := -0.072*p*l
	T2Cmin := -0.576*p*l
	T3Cmin := -0.091*p*l
	
	Mmax := M11max
	Mmin := MBmin
	Rmax := RBmax
	TRmax1 := -0.620*p*l
	TRmax2 := 0.598*p*l
	Rmin := RCmin
	Tmax := abs(TRmax1)
	Tmin := T2Cmin

}

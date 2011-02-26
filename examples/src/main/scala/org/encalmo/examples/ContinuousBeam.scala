package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator

object ContinuousBeamSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "beam"
	
	val p = symbol(BasicSymbols.p) unit "N/m"
	val l = symbol(BasicSymbols.l) unit "m"
	val P = symbol(BasicSymbols.P) unit "N"
	
	val lBA = symbol(BasicSymbols.l|"BA") unit "m"
	val lBC = symbol(BasicSymbols.l|"BC") unit "m"
	val lCB = symbol(BasicSymbols.l|"CB") unit "m"
	val lCD = symbol(BasicSymbols.l|"CD") unit "m"
	
	val Mmax = symbol(BasicSymbols.M|"max") unit "Nm"
	val Mmin = symbol(BasicSymbols.M|"min") unit "Nm"
	val Rmax = symbol(BasicSymbols.R|"max") unit "N"
	val TRmax1 = symbol(BasicSymbols.T|"Rmax,1") unit "N"
	val TRmax2 = symbol(BasicSymbols.T|"Rmax,2") unit "N"
	val Rmin = symbol(BasicSymbols.R|"min") unit "N"
	val Tmax = symbol(BasicSymbols.T|"max") unit "Nm"
	val Tmin = symbol(BasicSymbols.T|"min") unit "Nm"
	
	
	val M11max = symbol(BasicSymbols.M|"11,max") unit "Nm"
	val M12max = symbol(BasicSymbols.M|"12,max") unit "Nm"
	val M21max = symbol(BasicSymbols.M|"21,max") unit "Nm"
	val M22max = symbol(BasicSymbols.M|"22,max") unit "Nm"
	val M31max = symbol(BasicSymbols.M|"31,max") unit "Nm"
	val M32max = symbol(BasicSymbols.M|"32,max") unit "Nm"
	val MBmax = symbol(BasicSymbols.M|"B,max") unit "Nm"
	val MCmax = symbol(BasicSymbols.M|"C,max") unit "Nm"
	val RAmax = symbol(BasicSymbols.R|"A,max") unit "N"
	val RBmax = symbol(BasicSymbols.R|"B,max") unit "N"
	val RCmax = symbol(BasicSymbols.R|"C,max") unit "N"
	val T1Bmax = symbol(BasicSymbols.T|"1,B,max") unit "N"
	val T2Bmax = symbol(BasicSymbols.T|"2,B,max") unit "N"
	val T2Cmax = symbol(BasicSymbols.T|"2,C,max") unit "N"
	val T3Cmax = symbol(BasicSymbols.T|"3,C,max") unit "N"
	
	val M11min = symbol(BasicSymbols.M|"11,min") unit "Nm"
	val M12min = symbol(BasicSymbols.M|"12,min") unit "Nm"
	val M21min = symbol(BasicSymbols.M|"21,min") unit "Nm"
	val M22min = symbol(BasicSymbols.M|"22,min") unit "Nm"
	val M31min = symbol(BasicSymbols.M|"31,min") unit "Nm"
	val M32min = symbol(BasicSymbols.M|"32,min") unit "Nm"
	val MBmin = symbol(BasicSymbols.M|"B,min") unit "Nm"
	val MCmin = symbol(BasicSymbols.M|"C,min") unit "Nm"
	val RAmin = symbol(BasicSymbols.R|"A,min") unit "N"
	val RBmin = symbol(BasicSymbols.R|"B,min") unit "N"
	val RCmin = symbol(BasicSymbols.R|"C,min") unit "N"
	val T1Bmin = symbol(BasicSymbols.T|"1,B,min") unit "N"
	val T2Bmin = symbol(BasicSymbols.T|"2,B,min") unit "N"
	val T2Cmin = symbol(BasicSymbols.T|"2,C,min") unit "N"
	val T3Cmin = symbol(BasicSymbols.T|"3,C,min") unit "N"

}

class ContinuousBeam(id:String, length:Expression, load:Expression, force:Expression) extends Calculation(Option(id)) {
	
	import ContinuousBeamSymbols._
	
	this(p) = load
	this(l) = length
	this(P) = force

}

/** Continuous beam static analysis */
class ContinuousBeam_5_LinearLoad(id:String, length:Expression, load:Expression) extends ContinuousBeam(id,length,load,0) {

	import ContinuousBeamSymbols._
	
	this(lBA) = 0.2113*l
	this(lBC) = 0.2*l
	this(lCB) = 0.2113*l
	this(lCD) = 0.2105*l
	
	this(M11max) = 0.1*p*(l^2)
	this(M12max) = 0
	this(M21max) = 0.079*p*(l^2)
	this(M22max) = 0
	this(M31max) = 0.086*p*(l^2)
	this(M32max) = 0
	
	this(MBmax) = 0.014*p*(l^2)
	this(MCmax) = 0.032*p*(l^2)
	
	this(RAmax) = 0.447*p*l
	this(RBmax) = 1.218*p*l
	this(RCmax) = 1.167*p*l
	
	this(T1Bmax) = 0.014*p*l
	this(T2Bmax) = 0.598*p*l
	this(T2Cmax) = 0.103*p*l
	this(T3Cmax) = 0.591*p*l
	
	this(M11min) = 0
	this(M12min) = 0
	this(M21min) = 0
	this(M22min) = 0
	this(M31min) = 0
	this(M32min) = 0
	
	this(MBmin) = -0.120*p*(l^2)
	this(MCmin) = -0.111*p*(l^2)
	
	this(RAmin) = -0.053*p*l
	this(RBmin) = -0.086*p*l
	this(RCmin) = -0.194*p*l
	
	this(T1Bmin) = -0.62*p*l
	this(T2Bmin) = -0.072*p*l
	this(T2Cmin) = -0.576*p*l
	this(T3Cmin) = -0.091*p*l
	
	this(Mmax) = M11max
	this(Mmin) = MBmin
	this(Rmax) = RBmax
	this(TRmax1) = -0.620*p*l
	this(TRmax2) = 0.598*p*l
	this(Rmin) = RCmin
	this(Tmax) = abs(TRmax1)
	this(Tmin) = T2Cmin


}
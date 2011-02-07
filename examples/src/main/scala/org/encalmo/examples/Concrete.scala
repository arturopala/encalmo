package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation.Calculation
import scala.collection.mutable.LinkedHashMap

class Concrete(id:String) extends Calculation(Option(id)) {
	
	this(Concrete.C_class) = text(id)
	
}

object Concrete extends LinkedHashMap[String,Concrete] {
	
	import BasicSymbols._
	
	val C_class = SymbolLocalized("concrete class")
	val fck = f|"ck" is "charakterystyczna wytrzymałość walcowa na ściskanie betonu po 28 dniach" unit "Pa"
	val fckcube = f|"ck,cube" is "charakterystyczna wytrzymałość na ściskanie betonu po 28 dniach oznaczona na próbkach sześciennych" unit "Pa"
	val fcm = f|"cm" is "średnia wartość wytrzymałości walcowej betonu na ściskanie" unit "Pa"
	val fctm = f|"ctm" is "średnia wartość wytrzymałości betonu na rozciąganie osiowe" unit "Pa"
	val fctk005 = f|"ctk, 0,05" is "" unit "Pa"
	val fctk095 = f|"ctk, 0,95" is "" unit "Pa"
	val Ecm = E|"cm" is "sieczny moduł sprężystości betonu" unit "Pa"
	val epsic1 = epsiv|"c1" is "" unit "‰"
	val epsicu1 = epsiv|"cu1" is "" unit "‰"
	val epsic2 = epsiv|"c2" is "" unit "‰"
	val epsicu2 = epsiv|"cu2" is "" unit "‰"
	val epsic3 = epsiv|"c3" is "" unit "‰"
	val epsicu3 = epsiv|"cu3" is "" unit "‰"
	val n = BasicSymbols.n is ""
	
	val C_50_60 = new Concrete("C50/60"){
		this(fck) = 50E6
		this(fckcube) = 60E6
		this(fcm) = 58E6
		this(fctm) = 4.1E6
		this(fctk005) = 2.9E6
		this(fctk095) = 5.3E6
		this(Ecm) = 37E9
		this(epsic1) = 2.45
		this(epsicu1) = 3.5
		this(epsic2) = 2.0
		this(epsicu2) = 3.5
		this(epsic3) = 1.75
		this(epsicu3) = 3.5
		this(n) = 2.0
	}
	
	this(C_50_60.id.get) = C_50_60
	
}
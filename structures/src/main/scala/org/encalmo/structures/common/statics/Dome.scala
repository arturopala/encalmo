package org.encalmo.structures.common.statics

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

object DomeSymbols extends SymbolConfigurator{
    
    import BasicSymbols._
	val dictionary, contextId = "dome"
	    
	val r = BasicSymbols.r unit SI.m // promien rzutu kopuly
	val R = BasicSymbols.R unit SI.m // promien sfery kopuly
	val h = BasicSymbols.h unit SI.cm // wysokosc przekroju
	val φ = BasicSymbols.phiv unit SI.deg // kat pionowy
	val Θ = BasicSymbols.theta unit SI.deg // kat poziomy
	
	val r0 = BasicSymbols.r|0 unit SI.m // promien otwarcia kopuly
	val φ0 = BasicSymbols.phiv|0 unit SI.deg // kat otwarcia kopuly
	val φ1 = BasicSymbols.phiv|1 unit SI.deg // kat podparcia kopuly
	
	val g = BasicSymbols.g unit "kN/m2" // ciezar wlasny kopuly
	val s = BasicSymbols.g unit "kN/m2" // obciazenie sniegiem
	val w = BasicSymbols.g unit SI.kPa // obciazenie wiatrem
	
	val Nφ0 = N|"φ0" unit SI.kN // sila brzegowa styczna w otwarciu
	
	val Nφg = N|"φ,g" args(φ) unit SI.kN // sila poludnikowa od ciezaru wlasnego
	val NΘg = N|"Θ,g" args(Θ) unit SI.kN // sila rownoleznikowa od ciezaru wlasnego
	
	val Nφs = N|"φ,s" args(φ) unit SI.kN // sila poludnikowa od obciazenia sniegiem
	val NΘs = N|"Θ,s" args(Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
	
	val Nφw = N|"φ,w" args(φ,Θ) unit SI.kN // sila poludnikowa od obciazenia wiatrem
	val NφΘw = N|"φΘ,w" args(φ,Θ) unit SI.kN // sila scinajaca od obciazenia wiatrem
	val NΘw = N|"Θ,w" args(φ,Θ) unit SI.kN // sila rownoleznikowa od obciazenia sniegiem
	
	val H = BasicSymbols.H
	val S = BasicSymbols.S
    
}

object DomeExpressions extends MapContext {

	import DomeSymbols._
	
	this(R) = r/sin(φ1)
	this(φ0) = arcsin(r0/R)
	
	this(Nφg) = -g*R/(1+cos(φ)) // sila poludnikowa od ciezaru wlasnego
	this(NΘg) = -g*R*(cos(φ) - (1/(1+cos(φ)))) // sila rownoleznikowa od ciezaru wlasnego
	
	this(Nφs) = -(s*R)/2 // sila poludnikowa od obciazenia sniegiem
	this(NΘs) = -(s*R)/2 * cos(2*φ) // sila rownoleznikowa od obciazenia sniegiem
	
	this(Nφw) = w*R*(-2d/3+cos(φ)-1d/3*(cos(φ)^3))*(cos(φ)/(sin(φ)^3))*cos(Θ) // sila poludnikowa od obciazenia wiatrem
	this(NφΘw) = w*R*(-2d/3+cos(φ)-1d/3*(cos(φ)^3))*(1/(sin(φ)^3))*sin(Θ) // sila scinajaca od obciazenia wiatrem
	this(NΘw) = w*R*(2d/3+(sin(φ)^2)-2d/3*(cos(φ)^4))*(1/(sin(φ)^3))*cos(Θ) // sila rownoleznikowa od obciazenia sniegiem
	
}

class Dome(id:String) extends Calculation(Option(id)) {

	import DomeSymbols._
	this add DomeExpressions
	
}
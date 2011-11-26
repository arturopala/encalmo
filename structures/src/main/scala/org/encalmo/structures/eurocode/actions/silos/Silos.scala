package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.calculation.Eval
import org.encalmo.document._
import org.encalmo.structures.eurocode.concrete.Concrete
import org.encalmo.structures.eurocode.concrete.ReinforcingSteel
import org.encalmo.structures.eurocode.concrete.ConcreteSymbols

/** Silos symbols */
object SilosSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "silos"
	    
	//input geometry    
    lazy val t = symbol(BasicSymbols.t) unit "m"
    lazy val ts = symbol(BasicSymbols.t|BasicSymbols.s) unit "m"
    
    //calculated geometry
    lazy val dc = symbol(BasicSymbols.d|BasicSymbols.c) unit "m"
    lazy val A = symbol(BasicSymbols.A) unit "m2"
    lazy val U = symbol(BasicSymbols.U) unit "m"
    lazy val AU = symbol("A/U")
    lazy val beta = symbol(BasicSymbols.beta) unit "°"
    lazy val hh = symbol(BasicSymbols.h|BasicSymbols.h) unit "m"
    lazy val he = symbol(BasicSymbols.h|BasicSymbols.e) unit "m"
    lazy val htp = symbol(BasicSymbols.h|"tp") unit "m"
    lazy val ho = symbol(BasicSymbols.h|"o") unit "m"
    lazy val hc = symbol(BasicSymbols.h|"c") unit "m"
    lazy val hb = symbol(BasicSymbols.h|"b") unit "m"
    lazy val hcdc = symbol("hc/dc")
    
    //volumes
    lazy val V = symbol(BasicSymbols.V) unit "m3"
    lazy val Vh = symbol(BasicSymbols.V|BasicSymbols.h) unit "m3"
    lazy val Vc = symbol(BasicSymbols.V|BasicSymbols.c) unit "m3"
    //lazy val Vf = symbol(BasicSymbols.V|BasicSymbols.f) unit "m3"
    
    //weights
    lazy val W = symbol(BasicSymbols.W) unit "t"
    
    //filling symmetrical load
    lazy val z = symbol(BasicSymbols.z) unit "m"
    lazy val phf = symbol(BasicSymbols.p|"hf") args (z) unit "kPa"
    lazy val pwf = symbol(BasicSymbols.p|"wf") args (z) unit "kPa"
    lazy val pvf = symbol(BasicSymbols.p|"vf") args (z) unit "kPa"
    lazy val pvft = symbol(BasicSymbols.p|"vft") unit "kPa"
    lazy val pho = symbol(BasicSymbols.p|"ho") unit "kPa"
    lazy val YJ = symbol(BasicSymbols.Y|"J") args (z)
    lazy val YR = symbol(BasicSymbols.Y|"R") args (z)
    lazy val zo = symbol(BasicSymbols.z|"o") unit "m"
    lazy val n = symbol(BasicSymbols.n)
    lazy val phf1 = symbol(BasicSymbols.p|"hf,1") unit "kPa"
    lazy val phf2 = symbol(BasicSymbols.p|"hf,2") unit "kPa"
    lazy val phf3 = symbol(BasicSymbols.p|"hf,3") unit "kPa"
    lazy val phft = symbol(BasicSymbols.p|"hft") unit "kPa"
    lazy val zV = symbol(BasicSymbols.z|BasicSymbols.V) unit "m"
    lazy val nfzSk = symbol(BasicSymbols.n|"fzSk") args(z) unit "kN/m"
    lazy val nfzSkt = symbol(BasicSymbols.n|"fzSkt") unit "kN/m"
    
    //filling patch load
    lazy val ef = symbol(BasicSymbols.e|"f") unit "m"
    lazy val E = symbol(BasicSymbols.E)
    lazy val Cpf = symbol(BasicSymbols.C|"pf")
    lazy val ppf = symbol(BasicSymbols.p|"pf") args (z) unit "kPa"
    lazy val s = symbol(BasicSymbols.s) unit "m"
    lazy val ppfnc = symbol(BasicSymbols.p|"pf,nc") args (z) unit "kPa"
    lazy val ppfnc1 = symbol(BasicSymbols.p|"pf,nc,1") unit "kPa"
    lazy val ppfzp = symbol(BasicSymbols.p|"pf,zp") unit "kPa"
    lazy val Fpf1 = symbol(BasicSymbols.F|"pf,1") unit "kN"
    lazy val zp = symbol(BasicSymbols.z|"p") unit SI.m
    
    //discharge symmetrical load
	lazy val CS = symbol(BasicSymbols.C|"S")
	lazy val Ch = symbol(BasicSymbols.C|"h")
	lazy val Cw = symbol(BasicSymbols.C|"w")
	lazy val phe = symbol(BasicSymbols.p|"he") args (z) unit "kPa"
    lazy val pwe = symbol(BasicSymbols.p|"we") args (z) unit "kPa"
    lazy val phet = symbol(BasicSymbols.p|"het") unit "kPa"
    lazy val nezSk = symbol(BasicSymbols.n|"ezSk") args (z) unit "kN/m"
    lazy val nezSkt = symbol(BasicSymbols.n|"ezSkt") unit "kN/m"
    
    //discharge patch load
    lazy val Cpe = symbol(BasicSymbols.C|"pe")
    lazy val ppe = symbol(BasicSymbols.p|"pe") args (z) unit "kPa"
    lazy val ppenc = symbol(BasicSymbols.p|"pe,nc") args (z) unit "kPa"
    lazy val ppenc1 = symbol(BasicSymbols.p|"pe,nc,1") unit "kPa"
    lazy val ppezp = symbol(BasicSymbols.p|"pe,zp") unit "kPa"
    lazy val Fpe1 = symbol(BasicSymbols.F|"pe,1") unit "kN"
    
    //loads on silo hoppers
    lazy val x = symbol(BasicSymbols.x)
    lazy val Cb = symbol(BasicSymbols.C|"b")
    lazy val pv = symbol(BasicSymbols.p|"v") args (x) unit "kPa"
    lazy val pnf = symbol(BasicSymbols.p|"nf") args (x) unit "kPa"
    lazy val ptf = symbol(BasicSymbols.p|"tf") args (x) unit "kPa"
    lazy val nh = symbol(BasicSymbols.n|"h")
    lazy val muheff = symbol(BasicSymbols.mu|"heff")
    lazy val Ff = symbol(BasicSymbols.F|"f")
    lazy val Fe = symbol(BasicSymbols.F|"e")
    lazy val pne = symbol(BasicSymbols.p|"ne") args (x) unit "kPa"
    lazy val pte = symbol(BasicSymbols.p|"te") args (x) unit "kPa"
}

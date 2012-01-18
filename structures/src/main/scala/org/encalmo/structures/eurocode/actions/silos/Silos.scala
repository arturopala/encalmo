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
	/** Grubość ściany silosu */
    lazy val t = symbol(BasicSymbols.t) unit "mm"
    /** Grubość płyty dennej silosu */
    lazy val th = symbol(BasicSymbols.t|BasicSymbols.s) unit "mm"
    lazy val tr = symbol(BasicSymbols.t|BasicSymbols.s) unit "mm" is "Grubość blachy powłoki dachu"
    
    //calculated geometry
    /** Wymiar charakterystyczny rzutu poprzecznego silosu (wg Rysunku 1.1d [1991-4]) */
    lazy val dc = symbol(BasicSymbols.d|BasicSymbols.c) unit "mm" acc 1
    /** Pole rzutu przekroju poprzecznego silosu */
    lazy val A = symbol(BasicSymbols.A) unit "m2" acc 0.1
    /** Wewnętrzny obwód rzutu przekroju poprzecznego silosu */
    lazy val U = symbol(BasicSymbols.U) unit "mm" acc 1
    /** Stosunek pola rzutu silosu do jego obwodu */
    lazy val AU = symbol("A/U")
    /** Kąt nachylenia ściany leja względem pionu */
    lazy val beta = symbol(BasicSymbols.beta) unit "°" acc 0.1
    /** Wysokość ostrosłupa leja od wierzchołka do punktu przejściowego */
    lazy val hh = symbol(BasicSymbols.h|BasicSymbols.h) unit "mm" acc 1
    /** Wysokość ostrosłupa leja od wierzchołka do otworu wysypowego */
    lazy val he = symbol(BasicSymbols.h|BasicSymbols.e) unit "mm" acc 1
    /** Całkowita wysokość górnego stożka nasypowego */
    lazy val htp = symbol(BasicSymbols.h|"tp") unit "mm" acc 1
    /** Wysokość od powierzchni zastępczej do podstawy górnego stożka nasypowego */
    lazy val ho = symbol(BasicSymbols.h|"o") unit "mm" acc 1
    /** Wysokość pionowego segmentu ściany silosu od punktu przejściowego do powierzchni zastępczej */
    lazy val hc = symbol(BasicSymbols.h|"c") unit "mm" acc 1
    /** Całkowita wysokość silosu od wierzchołka leja do powierzchni zastępczej */
    lazy val hb = symbol(BasicSymbols.h|"b") unit "mm" acc 1
    /** Stosunek wysokości do średnicy komory silosu */
    lazy val hcdc = symbol("hc/dc")
    lazy val alpha = symbol(BasicSymbols.alpha) unit "°" acc 0.1 is "Kąt nachylenia stożka powłoki dachu"
    
    //volumes
    /** Pojemność całkowita silosu */
    lazy val V = symbol(BasicSymbols.V) unit "m3" acc 0.01
    /** Pojemność pionowej części silosu */
    lazy val Vh = symbol(BasicSymbols.V|BasicSymbols.h) unit "m3" acc 0.01
    /** Pojemność ostrosłupa leja */
    lazy val Vc = symbol(BasicSymbols.V|BasicSymbols.c) unit "m3" acc 0.01
    //lazy val Vf = symbol(BasicSymbols.V|BasicSymbols.f) unit "m3"
    lazy val S = symbol(BasicSymbols.S) unit "m2" acc 0.01 is "Powierzchnia płaszcza silosu"
    lazy val Sh = symbol(BasicSymbols.S|BasicSymbols.h) unit "m2" acc 0.01 is "Powierzchnia płaszcza komory"
    lazy val Sc = symbol(BasicSymbols.S|BasicSymbols.c) unit "m2" acc 0.01 is "Powierzchnia płaszcza leja"
    
    //weights
    /** Maksymalna masa składowanego materiału */
    lazy val W = symbol(BasicSymbols.W) unit "t" acc 0.1
    
    //filling symmetrical load
    lazy val z = symbol(BasicSymbols.z) unit "cm" acc 1
    lazy val phf = symbol(BasicSymbols.p|"hf") args (z) unit "kPa"
    lazy val pwf = symbol(BasicSymbols.p|"wf") args (z) unit "kPa"
    lazy val pvf = symbol(BasicSymbols.p|"vf") args (z) unit "kPa"
    lazy val pvft = symbol(BasicSymbols.p|"vft") unit "kPa"
    lazy val pho = symbol(BasicSymbols.p|"ho") unit "kPa"
    lazy val YJ = symbol(BasicSymbols.Y|"J") args (z)
    lazy val YR = symbol(BasicSymbols.Y|"R") args (z)
    lazy val zo = symbol(BasicSymbols.z|"o") unit "mm" acc 1
    lazy val n = symbol(BasicSymbols.n)
    lazy val phf1 = symbol(BasicSymbols.p|"hf,1") unit "kPa"
    lazy val phf2 = symbol(BasicSymbols.p|"hf,2") unit "kPa"
    lazy val phf3 = symbol(BasicSymbols.p|"hf,3") unit "kPa"
    lazy val phft = symbol(BasicSymbols.p|"hft") unit "kPa"
    lazy val zV = symbol(BasicSymbols.z|BasicSymbols.V) unit "mm" acc 1
    lazy val nfzSk = symbol(BasicSymbols.n|"fzSk") args(z) unit "kN/m"
    lazy val nfzSkt = symbol(BasicSymbols.n|"fzSkt") unit "kN/m"
    
    //filling patch load
    lazy val ef = symbol(BasicSymbols.e|"f") unit "mm" acc 1
    lazy val Ef = symbol(BasicSymbols.E|"f")
    lazy val Cpf = symbol(BasicSymbols.C|"pf")
    lazy val ppf = symbol(BasicSymbols.p|"pf") args (z) unit "kPa"
    lazy val s = symbol(BasicSymbols.s) unit "mm"  acc 1
    lazy val ppfnc = symbol(BasicSymbols.p|"pf,nc") args (z) unit "kPa"
    lazy val ppfnc1 = symbol(BasicSymbols.p|"pf,nc,1") unit "kPa"
    lazy val ppfzp = symbol(BasicSymbols.p|"pf,zp") unit "kPa"
    lazy val Fpf1 = symbol(BasicSymbols.F|"pf,1") unit "kN"
    lazy val zp = symbol(BasicSymbols.z|"p") unit SI.mm acc 1
    
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
    lazy val fiwh = symbol(BasicSymbols.phi|"wh") unit "°"
    lazy val epsilon = symbol(BasicSymbols.epsi) unit "°"
    lazy val pnf0 = symbol(BasicSymbols.p|"nf,0") unit "kPa"
    lazy val ptf0 = symbol(BasicSymbols.p|"tf,0") unit "kPa"
    lazy val pnf1 = symbol(BasicSymbols.p|"nf,1") unit "kPa"
    lazy val ptf1 = symbol(BasicSymbols.p|"tf,1") unit "kPa"
    lazy val pne0 = symbol(BasicSymbols.p|"ne,0") unit "kPa"
    lazy val pte0 = symbol(BasicSymbols.p|"te,0") unit "kPa"
    lazy val pne1 = symbol(BasicSymbols.p|"ne,1") unit "kPa"
    lazy val pte1 = symbol(BasicSymbols.p|"te,1") unit "kPa"
    
    //lazy val Gk = symbol(BasicSymbols.G|"k") unit "kN" is "Charakterystyczny ciężar własny silosu"
    lazy val Gck = symbol(BasicSymbols.G|"ck") unit "kN/m" is "Charakterystyczny ciężar własny płaszcza komory"
    lazy val Ghk = symbol(BasicSymbols.G|"hk") unit "kN" is "Charakterystyczny ciężar własny płaszcza leja"
    lazy val Grk = symbol(BasicSymbols.G|"rk") unit "kN" is "Charakterystyczny ciężar własny powłoki dachu"
    lazy val Gsk = symbol(BasicSymbols.G|"sk") unit "kN" is "Charakterystyczny ciężar własny pierścieni usztywniających"
    lazy val Gpk = symbol(BasicSymbols.G|"pk") unit "kN" is "Charakterystyczny ciężar własny pomostu roboczego"
    lazy val Quk = symbol(BasicSymbols.Q|"uk") unit "kN" is "Charakterystyczne obciążenie użytkowe"
    
    lazy val sk = symbol(BasicSymbols.s|"k") unit "kN/m2" acc 0.1 is "Wartość charakterystyczna obciążenia gruntu śniegiem"
    lazy val HM = symbol(BasicSymbols.H|"M") is "Wysokość nad poziomem morza dla lokalizacji obiektu"
    lazy val mi1 = symbol(BasicSymbols.mu|"1") is "Współczynnik kształtu dachu"
    lazy val Ce = symbol(BasicSymbols.C|"e") is "Współczynnik ekspozycji"
    lazy val Ct = symbol(BasicSymbols.C|"t") is "Współczynnik termiczny"
    lazy val sr = symbol(BasicSymbols.s|"r") unit "kN/m2" acc 0.1 is "Charakterystyczne obciążenie śniegiem dachu"
    lazy val Qsk = symbol(BasicSymbols.Q|"sk") unit "kN/m" acc 0.1 is "Charakterystyczny obciążenie śniegiem krawędzi komory silosu"
    
    lazy val vbo = symbol(BasicSymbols.v|"b,o") unit "m/s" acc 0.1 is "Bazowa prędkość wiatru dla strefy I"
    lazy val qbo = symbol(BasicSymbols.q|"b,o") unit "kN/m2" acc 0.1 is "Ciśnienie prędkości wiatru dla strefy I"
    lazy val vmz = symbol(BasicSymbols.v|"m") args(z) unit "m/s" acc 0.1 is "Średnia prędkość wiatru dla II kategorii terenu"
    lazy val crz = symbol(BasicSymbols.c|"r") acc 0.01 args(z) is "Współczynnik chropowatości terenu"
    lazy val coz = symbol(BasicSymbols.c|"o") acc 0.01 args(z) is "Współczynnik rzeźby terenu"
    lazy val cez = symbol(BasicSymbols.c|"e") acc 0.01 args(z) is "Współczynnik ekspozycji"
    lazy val qpz = symbol(BasicSymbols.q|"p") args(z) unit "kN/m2" is "Szczytowe ciśnienie prędkości wiatru"
    lazy val cf = symbol(BasicSymbols.c|"f") acc 0.01 is "Współczynnik siły aerodynamicznej (oporu aerodynamicznego)"
    lazy val wemax = symbol(BasicSymbols.w|"e,max") unit "kN/m2" is "Maksymalne ciśnienie zewnętrzne od wiatru działające na powłokę silosu"
    lazy val wi = symbol(BasicSymbols.w|"i") unit "kN/m2" is "Ciśnienie wewnętrzne od wiatru działające na powłokę silosu"
    lazy val ze = symbol(BasicSymbols.z|"e") is "Wysokość odniesienia dla ciśnienia zewnętrznego"
    lazy val cpe = symbol(BasicSymbols.c|"p,e") acc 0.01 is "Współczynnik ciśnienia zewnętrznego"
    lazy val cpi = symbol(BasicSymbols.c|"p,i") acc 0.01 is "Współczynnik ciśnienia wewnętrznego"
    lazy val Aref = symbol(BasicSymbols.A|"ref") unit SI.m2 is "Pole powierzchni odniesienia konstrukcji"
    lazy val cscd = symbol(BasicSymbols.c|"scd") acc 0.01 is "Współczynnik konstrukcyjny"
    lazy val Re = symbol(BasicSymbols.R|"e") is "Liczba Reynoldsa"
    lazy val Fw = symbol(BasicSymbols.F|"w") unit SI.kN is "Charakterystyczna siła oddziaływania wiatru na silos"
    
    lazy val omega = symbol(BasicSymbols.omega) is "Parametr długości wyboczeniowej"
    lazy val Cx = symbol(BasicSymbols.C|"x") is "Współczynnik"
    lazy val sigxRcr = symbol(BasicSymbols.sigma|"x,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ściskaniu południkowym"
    lazy val alphax = symbol(BasicSymbols.alpha|"x") is "Parametr imperfekcji"
    lazy val dwk = symbol(BasicSymbols.delta|"wk") is "Amplituda reprezentatywnej imperfekcji"
    lazy val Qx = symbol(BasicSymbols.Q|"x") is "Parametr jakości"
    lazy val betax = symbol(BasicSymbols.beta|"x") is "Współczynnik zakresu plastycznego"
    lazy val etax = symbol(BasicSymbols.eta|"x") is "Wykładnik interakcji"
    lazy val lambdax0 = symbol(BasicSymbols.lambda|"x,0") is "Dolna smukłość graniczna"
    lazy val lambdax = symbol(BasicSymbols.lambda|"x") is "Smukłość dla ściskania południkowego"
    lazy val lambdapx = symbol(BasicSymbols.lambda|"p,x") is "Górna smukłość graniczna"
    lazy val sigxRd = symbol(BasicSymbols.sigma|"x,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ściskanie południkowe przy wyboczeniu"
    
    lazy val Cteta = symbol(BasicSymbols.C|BasicSymbols.theta) is "Współczynnik"
    lazy val sigtRcr = symbol(BasicSymbols.sigma|"θ,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ściskaniu równoleżnikowym"
    lazy val alphat = symbol(BasicSymbols.alpha|BasicSymbols.theta) is "Parametr imperfekcji"
    lazy val betat = symbol(BasicSymbols.beta|BasicSymbols.theta) is "Współczynnik zakresu plastycznego"
    lazy val etat = symbol(BasicSymbols.eta|BasicSymbols.theta) is "Wykładnik interakcji"
    lazy val lambdat0 = symbol(BasicSymbols.lambda|"θ,0") is "Dolna smukłość graniczna"
    lazy val kw = symbol(BasicSymbols.k|"w") is "Współczynnik zastępczy do obliczenia równomiernie rozłożonego parcia wiatru"
    lazy val lambdat = symbol(BasicSymbols.lambda|"θ") is "Smukłość dla ściskania równoleżnikowego"
    lazy val lambdapt = symbol(BasicSymbols.lambda|"p,θ") is "Górna smukłość graniczna"
    lazy val sigtRd = symbol(BasicSymbols.sigma|"θ,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ściskanie równoleżnikowe przy wyboczeniu"
    
    lazy val Ctau = symbol(BasicSymbols.C|BasicSymbols.tau) is "Współczynnik"
    lazy val tauxtRcr = symbol(BasicSymbols.tau|"xθ,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ścinaniu"
    lazy val alphatau = symbol(BasicSymbols.alpha|BasicSymbols.tau) is "Parametr imperfekcji"
    lazy val betatau = symbol(BasicSymbols.beta|BasicSymbols.tau) is "Współczynnik zakresu plastycznego"
    lazy val etatau = symbol(BasicSymbols.eta|BasicSymbols.tau) is "Wykładnik interakcji"
    lazy val lambdatau0 = symbol(BasicSymbols.lambda|"τ,0") is "Dolna smukłość graniczna"
    lazy val lambdatau = symbol(BasicSymbols.lambda|"τ") is "Smukłość dla ścinania"
    lazy val lambdaptau = symbol(BasicSymbols.lambda|"p,τ") is "Górna smukłość graniczna"
    lazy val tauRd = symbol(BasicSymbols.tau|"xθ,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ścinanie przy wyboczeniu"
    
    lazy val chix = symbol(BasicSymbols.chi|"x") is "Współczynnik wyboczeniowy dla ściskania południkowego"
    lazy val chiteta = symbol(BasicSymbols.chi|"θ") is "Współczynnik wyboczeniowy dla ściskania równoleżnikowego"
    lazy val chitau = symbol(BasicSymbols.chi|"τ") is "Współczynnik wyboczeniowy dla ścinania"
    
    lazy val sigmaxEd = symbol(BasicSymbols.sigma|"x,Ed") unit SI.MPa is "Obliczeniowe naprężenie południkowe"
    lazy val sigmatEd = symbol(BasicSymbols.sigma|"θ,Ed") unit SI.MPa is "Obliczeniowe naprężenie równoleżnikowe"
    lazy val tauEd = symbol(BasicSymbols.tau|"xθ,Ed") unit SI.MPa is "Obliczeniowe naprężenie ścinające"
    
    lazy val lR = symbol(BasicSymbols.ls|"R") unit SI.mm acc 1 is "Długość strefy przypodporowej wg [1993-1-6] D.1.6 (2)"
    lazy val lf = symbol(BasicSymbols.ls|"f") unit SI.mm acc 1 is "Długość strefy płaszcza komory narażonej na wyboczenie wg [1993-1-6] D.52"
    
    lazy val qwzd = symbol(BasicSymbols.q|"wz,d") unit "kN/m2" is "Zastępcze obciążenie obliczeniowe od działania wiatru"
    lazy val NxEd = symbol(BasicSymbols.N|"x,Ed") unit SI.kN is "Obliczeniowa siła południkowa"
    lazy val MwEd = symbol(BasicSymbols.M|"w,Ed") unit SI.kNm is "Obliczeniowy moment zginający od działania wiatru"
    lazy val QtauEd = symbol(BasicSymbols.Q|"τ,Ed") unit SI.kN is "Obliczeniowa siła ścinająca"
    
    lazy val kx = symbol(BasicSymbols.k|"x") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.46"
    lazy val kt = symbol(BasicSymbols.k|"θ") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.47"
    lazy val ktau = symbol(BasicSymbols.k|"τ") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.48"
    lazy val ki = symbol(BasicSymbols.k|"i") acc 0.001 is "Współczynnik interakcji wg [1993-1-6] D.49"
    
    lazy val zeta = symbol(BasicSymbols.zeta) acc 0.001 is "Wskaźnik interakcji naprężeń południkowych, równoleżnikowych i ścinających wg [1993-1-6] 8.5.3 (3)"
    
    lazy val s1 = symbol(BasicSymbols.s|"1") unit SI.mm acc 1 is "Szerokość słupa podpierającego"
    lazy val hpp = symbol(BasicSymbols.h|"pp") unit SI.mm acc 1 is "Wysokość pierścienia podporowego"
    lazy val tp = symbol(BasicSymbols.t|"p") unit SI.mm acc 1 is "Grubość blachy płaszcza w strefie podporowej"
    lazy val tpp = symbol(BasicSymbols.t|"pp") unit SI.mm acc 1 is "Grubość środnika pierścienia podporowego"
    lazy val ns = symbol(BasicSymbols.n|"s") acc 1 is "Liczba podpór silosu"
    lazy val s2 = symbol(BasicSymbols.s|"2") unit SI.mm acc 1 is "Szerokość płaszcza komory obciążona lokalnie przy podporze"
    
    lazy val sigmax1 = symbol(BasicSymbols.sigma|"x,1") unit SI.MPa is "Obliczeniowe naprężenie na dolnej krawędzi pierścienia podporowego"
    lazy val sigmax2 = symbol(BasicSymbols.sigma|"x,2") unit SI.MPa is "Obliczeniowe naprężenie na dolnej krawędzi płaszcza komory"
    lazy val sigmax1cr = symbol(BasicSymbols.sigma|"x,1,cr") unit SI.MPa is "Obliczeniowe naprężenie krytyczne w strefie przypodporowej"
    lazy val sigmax2cr = symbol(BasicSymbols.sigma|"x,2,cr") unit SI.MPa is "Obliczeniowe naprężenie krytyczne w strefie przypodporowej"
    lazy val N1 = symbol(BasicSymbols.N|"1") unit SI.kN is "Obliczeniowa siła działająca na słup podporowy"
    
    lazy val lambdax1 = symbol(BasicSymbols.lambda|"x,1") is "Smukłość dla ściskania południkowego pierscienia podporowego"
    lazy val sigxRd1 = symbol(BasicSymbols.sigma|"x,Rd,1") unit SI.MPa is "Obliczeniowa wytrzymałość pierscienia podporowego na ściskanie południkowe przy wyboczeniu"
    lazy val chix1 = symbol(BasicSymbols.chi|"x,1") is "Współczynnik wyboczeniowy dla ściskania południkowego pierscienia podporowego"
    
    lazy val MeEd = symbol(BasicSymbols.M|"e,Ed") unit SI.kNm is "Obliczeniowy moment zginający od parcia lokalnego przy opróżnianiu"
}

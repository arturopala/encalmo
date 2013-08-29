package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._

/** Silos symbols */
trait SiloSymbols extends SymbolConfigurator {
	    
	//input geometry    
	/** Grubość ściany silosu */
    val t = symbol(BasicSymbols.t) unit "mm"
    /** Grubość płyty dennej silosu */
    val th = symbol(BasicSymbols.t|BasicSymbols.s) unit "mm"
    val tr = symbol(BasicSymbols.t|BasicSymbols.s) unit "mm" is "Grubość blachy powłoki dachu"
    
    //calculated geometry
    /** Wymiar charakterystyczny rzutu poprzecznego silosu (wg Rysunku 1.1d [1991-4]) */
    val dc = symbol(BasicSymbols.d|BasicSymbols.c) unit "mm" acc 1
    /** Pole rzutu przekroju poprzecznego silosu */
    val A = symbol(BasicSymbols.A) unit "m2" acc 0.1
    /** Wewnętrzny obwód rzutu przekroju poprzecznego silosu */
    val U = symbol(BasicSymbols.U) unit "mm" acc 1
    /** Stosunek pola rzutu silosu do jego obwodu */
    val AU = symbol("A/U")
    /** Kąt nachylenia ściany leja względem pionu */
    val beta = symbol(BasicSymbols.beta) unit "°" acc 0.1
    /** Wysokość ostrosłupa leja od wierzchołka do punktu przejściowego */
    val hh = symbol(BasicSymbols.h|BasicSymbols.h) unit "mm" acc 1
    /** Wysokość ostrosłupa leja od wierzchołka do otworu wysypowego */
    val he = symbol(BasicSymbols.h|BasicSymbols.e) unit "mm" acc 1
    /** Całkowita wysokość górnego stożka nasypowego */
    val htp = symbol(BasicSymbols.h|"tp") unit "mm" acc 1
    /** Wysokość od powierzchni zastępczej do podstawy górnego stożka nasypowego */
    val ho = symbol(BasicSymbols.h|"o") unit "mm" acc 1
    /** Wysokość pionowego segmentu ściany silosu od punktu przejściowego do powierzchni zastępczej */
    val hc = symbol(BasicSymbols.h|"c") unit "mm" acc 1
    /** Całkowita wysokość silosu od wierzchołka leja do powierzchni zastępczej */
    val hb = symbol(BasicSymbols.h|"b") unit "mm" acc 1
    /** Stosunek wysokości do średnicy komory silosu */
    val hcdc = symbol("hc/dc")
    val alpha = symbol(BasicSymbols.alpha) unit "°" acc 0.1 is "Kąt nachylenia stożka powłoki dachu"
    
    //volumes
    /** Pojemność całkowita silosu */
    val V = symbol(BasicSymbols.V) unit "m3" acc 0.01
    /** Pojemność pionowej części silosu */
    val Vh = symbol(BasicSymbols.V|BasicSymbols.h) unit "m3" acc 0.01
    /** Pojemność ostrosłupa leja */
    val Vc = symbol(BasicSymbols.V|BasicSymbols.c) unit "m3" acc 0.01
    //val Vf = symbol(BasicSymbols.V|BasicSymbols.f) unit "m3"
    val S = symbol(BasicSymbols.S) unit "m2" acc 0.01 is "Powierzchnia płaszcza silosu"
    val Sh = symbol(BasicSymbols.S|BasicSymbols.h) unit "m2" acc 0.01 is "Powierzchnia płaszcza leja"
    val Sc = symbol(BasicSymbols.S|BasicSymbols.c) unit "m2" acc 0.01 is "Powierzchnia płaszcza komory"
    
    //weights
    /** Maksymalna masa składowanego materiału */
    val W = symbol(BasicSymbols.W) unit "t" acc 0.1
    
    //filling symmetrical load
    val z = symbol(BasicSymbols.z) unit "cm" acc 1
    val phf = symbol(BasicSymbols.p|"hf") args (z) unit "kPa"
    val pwf = symbol(BasicSymbols.p|"wf") args (z) unit "kPa"
    val pvf = symbol(BasicSymbols.p|"vf") args (z) unit "kPa"
    val pvft = symbol(BasicSymbols.p|"vft") unit "kPa"
    val pho = symbol(BasicSymbols.p|"ho") unit "kPa"
    val YJ = symbol(BasicSymbols.Y|"J") args (z)
    val YR = symbol(BasicSymbols.Y|"R") args (z)
    val zo = symbol(BasicSymbols.z|"o") unit "mm" acc 1
    val n = symbol(BasicSymbols.n)
    val phf1 = symbol(BasicSymbols.p|"hf,1") unit "kPa"
    val phf2 = symbol(BasicSymbols.p|"hf,2") unit "kPa"
    val phf3 = symbol(BasicSymbols.p|"hf,3") unit "kPa"
    val phft = symbol(BasicSymbols.p|"hft") unit "kPa"
    val zV = symbol(BasicSymbols.z|BasicSymbols.V) unit "mm" acc 1
    val nfzSk = symbol(BasicSymbols.n|"fzSk") args(z) unit "kN/m"
    val nfzSkt = symbol(BasicSymbols.n|"fzSkt") unit "kN/m"
    
    //filling patch load
    val ef = symbol(BasicSymbols.e|"f") unit "mm" acc 1
    val Ef = symbol(BasicSymbols.E|"f")
    val Cpf = symbol(BasicSymbols.C|"pf")
    val ppf = symbol(BasicSymbols.p|"pf") args (z) unit "kPa"
    val s = symbol(BasicSymbols.s) unit "mm"  acc 1
    val ppfnc = symbol(BasicSymbols.p|"pf,nc") args (z) unit "kPa"
    val ppfnc1 = symbol(BasicSymbols.p|"pf,nc,1") unit "kPa"
    val ppfzp = symbol(BasicSymbols.p|"pf,zp") unit "kPa"
    val Fpf1 = symbol(BasicSymbols.F|"pf,1") unit "kN"
    val zp = symbol(BasicSymbols.z|"p") unit SI.mm acc 1
    
    //discharge symmetrical load
	val CS = symbol(BasicSymbols.C|"S")
	val Ch = symbol(BasicSymbols.C|"h")
	val Cw = symbol(BasicSymbols.C|"w")
	val phe = symbol(BasicSymbols.p|"he") args (z) unit "kPa"
    val pwe = symbol(BasicSymbols.p|"we") args (z) unit "kPa"
    val phet = symbol(BasicSymbols.p|"het") unit "kPa"
    val nezSk = symbol(BasicSymbols.n|"ezSk") args (z) unit "kN/m"
    val nezSkt = symbol(BasicSymbols.n|"ezSkt") unit "kN/m"
    
    //discharge patch load
    val Cpe = symbol(BasicSymbols.C|"pe")
    val ppe = symbol(BasicSymbols.p|"pe") args (z) unit "kPa"
    val ppenc = symbol(BasicSymbols.p|"pe,nc") args (z) unit "kPa"
    val ppenc1 = symbol(BasicSymbols.p|"pe,nc,1") unit "kPa"
    val ppezp = symbol(BasicSymbols.p|"pe,zp") unit "kPa"
    val Fpe1 = symbol(BasicSymbols.F|"pe,1") unit "kN"
    
    //loads on silo hoppers
    val x = symbol(BasicSymbols.x)
    val Cb = symbol(BasicSymbols.C|"b")
    val pv = symbol(BasicSymbols.p|"v") args (x) unit "kPa"
    val pnf = symbol(BasicSymbols.p|"nf") args (x) unit "kPa"
    val ptf = symbol(BasicSymbols.p|"tf") args (x) unit "kPa"
    val nh = symbol(BasicSymbols.n|"h")
    val muheff = symbol(BasicSymbols.mu|"heff")
    val Ff = symbol(BasicSymbols.F|"f")
    val Fe = symbol(BasicSymbols.F|"e")
    val pne = symbol(BasicSymbols.p|"ne") args (x) unit "kPa"
    val pte = symbol(BasicSymbols.p|"te") args (x) unit "kPa"
    val fiwh = symbol(BasicSymbols.phi|"wh") unit "°"
    val epsilon = symbol(BasicSymbols.epsi) unit "°"
    val pnf0 = symbol(BasicSymbols.p|"nf,0") unit "kPa"
    val ptf0 = symbol(BasicSymbols.p|"tf,0") unit "kPa"
    val pnf1 = symbol(BasicSymbols.p|"nf,1") unit "kPa"
    val ptf1 = symbol(BasicSymbols.p|"tf,1") unit "kPa"
    val pne0 = symbol(BasicSymbols.p|"ne,0") unit "kPa"
    val pte0 = symbol(BasicSymbols.p|"te,0") unit "kPa"
    val pne1 = symbol(BasicSymbols.p|"ne,1") unit "kPa"
    val pte1 = symbol(BasicSymbols.p|"te,1") unit "kPa"
    
    //val Gk = symbol(BasicSymbols.G|"k") unit "kN" is "Charakterystyczny ciężar własny silosu"
    val Gck = symbol(BasicSymbols.G|"ck") unit "kN/m" is "Charakterystyczny ciężar własny płaszcza komory"
    val Ghk = symbol(BasicSymbols.G|"hk") unit "kN" is "Charakterystyczny ciężar własny płaszcza leja"
    val Grk = symbol(BasicSymbols.G|"rk") unit "kN" is "Charakterystyczny ciężar własny powłoki dachu"
    val Gsk = symbol(BasicSymbols.G|"sk") unit "kN" is "Charakterystyczny ciężar własny pierścieni usztywniających"
    val Gpk = symbol(BasicSymbols.G|"pk") unit "kN" is "Charakterystyczny ciężar własny pomostu roboczego"
    val Quk = symbol(BasicSymbols.Q|"uk") unit "kN" is "Charakterystyczne obciążenie użytkowe"
    
    val sk = symbol(BasicSymbols.s|"k") unit "kN/m2" acc 0.1 is "Wartość charakterystyczna obciążenia gruntu śniegiem"
    val HM = symbol(BasicSymbols.H|"M") is "Wysokość nad poziomem morza dla lokalizacji obiektu"
    val mi1 = symbol(BasicSymbols.mu|"1") is "Współczynnik kształtu dachu"
    val Ce = symbol(BasicSymbols.C|"e") is "Współczynnik ekspozycji"
    val Ct = symbol(BasicSymbols.C|"t") is "Współczynnik termiczny"
    val sr = symbol(BasicSymbols.s|"r") unit "kN/m2" acc 0.1 is "Charakterystyczne obciążenie śniegiem dachu"
    val Qsk = symbol(BasicSymbols.Q|"sk") unit "kN/m" acc 0.1 is "Charakterystyczny obciążenie śniegiem krawędzi komory silosu"
    
    val vbo = symbol(BasicSymbols.v|"b,o") unit "m/s" acc 0.1 is "Bazowa prędkość wiatru dla strefy I"
    val qbo = symbol(BasicSymbols.q|"b,o") unit "kN/m2" acc 0.1 is "Ciśnienie prędkości wiatru dla strefy I"
    val vmz = symbol(BasicSymbols.v|"m") args(z) unit "m/s" acc 0.1 is "Średnia prędkość wiatru dla II kategorii terenu"
    val crz = symbol(BasicSymbols.c|"r") acc 0.01 args(z) is "Współczynnik chropowatości terenu"
    val coz = symbol(BasicSymbols.c|"o") acc 0.01 args(z) is "Współczynnik rzeźby terenu"
    val cez = symbol(BasicSymbols.c|"e") acc 0.01 args(z) is "Współczynnik ekspozycji"
    val qpz = symbol(BasicSymbols.q|"p") args(z) unit "kN/m2" is "Szczytowe ciśnienie prędkości wiatru"
    val cf = symbol(BasicSymbols.c|"f") acc 0.01 is "Współczynnik siły aerodynamicznej (oporu aerodynamicznego)"
    val wemax = symbol(BasicSymbols.w|"e,max") unit "kN/m2" is "Maksymalne ciśnienie zewnętrzne od wiatru działające na powłokę silosu"
    val wi = symbol(BasicSymbols.w|"i") unit "kN/m2" is "Ciśnienie wewnętrzne od wiatru działające na powłokę silosu"
    val ze = symbol(BasicSymbols.z|"e") is "Wysokość odniesienia dla ciśnienia zewnętrznego"
    val cpe = symbol(BasicSymbols.c|"p,e") acc 0.01 is "Współczynnik ciśnienia zewnętrznego"
    val cpi = symbol(BasicSymbols.c|"p,i") acc 0.01 is "Współczynnik ciśnienia wewnętrznego"
    val Aref = symbol(BasicSymbols.A|"ref") unit SI.m2 is "Pole powierzchni odniesienia konstrukcji"
    val cscd = symbol(BasicSymbols.c|"scd") acc 0.01 is "Współczynnik konstrukcyjny"
    val Re = symbol(BasicSymbols.R|"e") is "Liczba Reynoldsa"
    val Fw = symbol(BasicSymbols.F|"w") unit SI.kN is "Charakterystyczna siła oddziaływania wiatru na silos"
    
    val omega = symbol(BasicSymbols.omega) is "Parametr długości wyboczeniowej"
    val Cx = symbol(BasicSymbols.C|"x") is "Współczynnik"
    val sigxRcr = symbol(BasicSymbols.sigma|"x,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ściskaniu południkowym"
    val alphax = symbol(BasicSymbols.alpha|"x") is "Parametr imperfekcji"
    val dwk = symbol(BasicSymbols.delta|"wk") is "Amplituda reprezentatywnej imperfekcji"
    val Qx = symbol(BasicSymbols.Q|"x") is "Parametr jakości"
    val betax = symbol(BasicSymbols.beta|"x") is "Współczynnik zakresu plastycznego"
    val etax = symbol(BasicSymbols.eta|"x") is "Wykładnik interakcji"
    val lambdax0 = symbol(BasicSymbols.lambda|"x,0") is "Dolna smukłość graniczna"
    val lambdax = symbol(BasicSymbols.lambda|"x") is "Smukłość dla ściskania południkowego"
    val lambdapx = symbol(BasicSymbols.lambda|"p,x") is "Górna smukłość graniczna"
    val sigxRd = symbol(BasicSymbols.sigma|"x,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ściskanie południkowe przy wyboczeniu"
    
    val Cteta = symbol(BasicSymbols.C|BasicSymbols.theta) is "Współczynnik"
    val sigtRcr = symbol(BasicSymbols.sigma|"θ,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ściskaniu równoleżnikowym"
    val alphat = symbol(BasicSymbols.alpha|BasicSymbols.theta) is "Parametr imperfekcji"
    val betat = symbol(BasicSymbols.beta|BasicSymbols.theta) is "Współczynnik zakresu plastycznego"
    val etat = symbol(BasicSymbols.eta|BasicSymbols.theta) is "Wykładnik interakcji"
    val lambdat0 = symbol(BasicSymbols.lambda|"θ,0") is "Dolna smukłość graniczna"
    val kw = symbol(BasicSymbols.k|"w") is "Współczynnik zastępczy do obliczenia równomiernie rozłożonego parcia wiatru"
    val lambdat = symbol(BasicSymbols.lambda|"θ") is "Smukłość dla ściskania równoleżnikowego"
    val lambdapt = symbol(BasicSymbols.lambda|"p,θ") is "Górna smukłość graniczna"
    val sigtRd = symbol(BasicSymbols.sigma|"θ,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ściskanie równoleżnikowe przy wyboczeniu"
    
    val Ctau = symbol(BasicSymbols.C|BasicSymbols.tau) is "Współczynnik"
    val tauxtRcr = symbol(BasicSymbols.tau|"xθ,Rcr") unit SI.MPa is "Naprężenie krytyczne przy ścinaniu"
    val alphatau = symbol(BasicSymbols.alpha|BasicSymbols.tau) is "Parametr imperfekcji"
    val betatau = symbol(BasicSymbols.beta|BasicSymbols.tau) is "Współczynnik zakresu plastycznego"
    val etatau = symbol(BasicSymbols.eta|BasicSymbols.tau) is "Wykładnik interakcji"
    val lambdatau0 = symbol(BasicSymbols.lambda|"τ,0") is "Dolna smukłość graniczna"
    val lambdatau = symbol(BasicSymbols.lambda|"τ") is "Smukłość dla ścinania"
    val lambdaptau = symbol(BasicSymbols.lambda|"p,τ") is "Górna smukłość graniczna"
    val tauRd = symbol(BasicSymbols.tau|"xθ,Rd") unit SI.MPa is "Obliczeniowa wytrzymałość na ścinanie przy wyboczeniu"
    
    val chix = symbol(BasicSymbols.chi|"x") is "Współczynnik wyboczeniowy dla ściskania południkowego"
    val chiteta = symbol(BasicSymbols.chi|"θ") is "Współczynnik wyboczeniowy dla ściskania równoleżnikowego"
    val chitau = symbol(BasicSymbols.chi|"τ") is "Współczynnik wyboczeniowy dla ścinania"
    
    val sigmaxEd = symbol(BasicSymbols.sigma|"x,Ed") unit SI.MPa is "Obliczeniowe naprężenie południkowe"
    val sigmatEd = symbol(BasicSymbols.sigma|"θ,Ed") unit SI.MPa is "Obliczeniowe naprężenie równoleżnikowe"
    val tauEd = symbol(BasicSymbols.tau|"xθ,Ed") unit SI.MPa is "Obliczeniowe naprężenie ścinające"
    
    val lR = symbol(BasicSymbols.ls|"R") unit SI.mm acc 1 is "Długość strefy przypodporowej wg [1993-1-6] D.1.6 (2)"
    val lf = symbol(BasicSymbols.ls|"f") unit SI.mm acc 1 is "Długość strefy płaszcza komory narażonej na wyboczenie wg [1993-1-6] D.52"
    
    val qwzd = symbol(BasicSymbols.q|"wz,d") unit "kN/m2" is "Zastępcze obciążenie obliczeniowe od działania wiatru"
    val NxEd = symbol(BasicSymbols.N|"x,Ed") unit SI.kN is "Obliczeniowa siła południkowa"
    val MwEd = symbol(BasicSymbols.M|"w,Ed") unit SI.kNm is "Obliczeniowy moment zginający od działania wiatru"
    val QtauEd = symbol(BasicSymbols.Q|"τ,Ed") unit SI.kN is "Obliczeniowa siła ścinająca"
    
    val kx = symbol(BasicSymbols.k|"x") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.46"
    val kt = symbol(BasicSymbols.k|"θ") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.47"
    val ktau = symbol(BasicSymbols.k|"τ") acc 0.001 is "Wykładnik interakcji wg [1993-1-6] D.48"
    val ki = symbol(BasicSymbols.k|"i") acc 0.001 is "Współczynnik interakcji wg [1993-1-6] D.49"
    
    val zeta = symbol(BasicSymbols.zeta) acc 0.001 is "Wskaźnik interakcji naprężeń południkowych, równoleżnikowych i ścinających wg [1993-1-6] 8.5.3 (3)"
    
    val s1 = symbol(BasicSymbols.s|"1") unit SI.mm acc 1 is "Szerokość słupa podpierającego"
    val hpp = symbol(BasicSymbols.h|"pp") unit SI.mm acc 1 is "Wysokość pierścienia podporowego"
    val tp = symbol(BasicSymbols.t|"p") unit SI.mm acc 1 is "Grubość blachy płaszcza w strefie podporowej"
    val tpp = symbol(BasicSymbols.t|"pp") unit SI.mm acc 1 is "Grubość środnika pierścienia podporowego"
    val ns = symbol(BasicSymbols.n|"s") acc 1 is "Liczba podpór silosu"
    val s2 = symbol(BasicSymbols.s|"2") unit SI.mm acc 1 is "Szerokość płaszcza komory obciążona lokalnie przy podporze"
    
    val sigmax1 = symbol(BasicSymbols.sigma|"x,1") unit SI.MPa is "Obliczeniowe naprężenie na dolnej krawędzi pierścienia podporowego"
    val sigmax2 = symbol(BasicSymbols.sigma|"x,2") unit SI.MPa is "Obliczeniowe naprężenie na dolnej krawędzi płaszcza komory"
    val sigmax1cr = symbol(BasicSymbols.sigma|"x,1,cr") unit SI.MPa is "Obliczeniowe naprężenie krytyczne w strefie przypodporowej"
    val sigmax2cr = symbol(BasicSymbols.sigma|"x,2,cr") unit SI.MPa is "Obliczeniowe naprężenie krytyczne w strefie przypodporowej"
    val N1 = symbol(BasicSymbols.N|"1") unit SI.kN is "Obliczeniowa siła działająca na słup podporowy"
    
    val lambdax1 = symbol(BasicSymbols.lambda|"x,1") is "Smukłość dla ściskania południkowego pierscienia podporowego"
    val sigxRd1 = symbol(BasicSymbols.sigma|"x,Rd,1") unit SI.MPa is "Obliczeniowa wytrzymałość pierscienia podporowego na ściskanie południkowe przy wyboczeniu"
    val chix1 = symbol(BasicSymbols.chi|"x,1") is "Współczynnik wyboczeniowy dla ściskania południkowego pierscienia podporowego"
    
    val MeEd = symbol(BasicSymbols.M|"e,Ed") unit SI.kNm is "Obliczeniowy moment zginający od parcia lokalnego przy opróżnianiu"
}

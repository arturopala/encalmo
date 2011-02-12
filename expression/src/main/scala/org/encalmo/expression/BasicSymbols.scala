package org.encalmo.expression

/**
 * Basic latin symbols set
 * @author artur.opala
 */
object BasicSymbols {
	
	val a:Symbol = Symbol("a")
  	val b:Symbol = Symbol("b")
	val c:Symbol = Symbol("c")
	val d:Symbol = Symbol("d")
	val e:Symbol = Symbol("e")
	val f:Symbol = Symbol("f")
	val g:Symbol = Symbol("g")
	val h:Symbol = Symbol("h")
	val i:Symbol = Symbol("i")
	val j:Symbol = Symbol("j")
  	val k:Symbol = Symbol("k")
	val l:Symbol = Symbol("l")
	val m:Symbol = Symbol("m")
	val n:Symbol = Symbol("n")
	val o:Symbol = Symbol("o")
	val p:Symbol = Symbol("p")
	val q:Symbol = Symbol("q")
	val r:Symbol = Symbol("r")
	val s:Symbol = Symbol("s")
	val t:Symbol = Symbol("t")
	val u:Symbol = Symbol("u")
	val v:Symbol = Symbol("v")
	val w:Symbol = Symbol("w")
	val x:Symbol = Symbol("x")
	val y:Symbol = Symbol("y")
	val z:Symbol = Symbol("z")
	
	val A:Symbol = Symbol("A")
  	val B:Symbol = Symbol("B")
	val C:Symbol = Symbol("C")
	val D:Symbol = Symbol("D")
	val E:Symbol = Symbol("E")
	val F:Symbol = Symbol("F")
	val G:Symbol = Symbol("G")
	val H:Symbol = Symbol("H")
	val I:Symbol = Symbol("I")
	val J:Symbol = Symbol("J")
  	val K:Symbol = Symbol("K")
	val L:Symbol = Symbol("L")
	val M:Symbol = Symbol("M")
	val N:Symbol = Symbol("N")
	val O:Symbol = Symbol("O")
	val P:Symbol = Symbol("P")
	val Q:Symbol = Symbol("Q")
	val R:Symbol = Symbol("R")
	val S:Symbol = Symbol("S")
	val T:Symbol = Symbol("T")
	val U:Symbol = Symbol("U")
	val V:Symbol = Symbol("V")
	val W:Symbol = Symbol("W")
	val X:Symbol = Symbol("X")
	val Y:Symbol = Symbol("Y")
	val Z:Symbol = Symbol("Z")
	
	val alpha:Symbol = Symbol("alpha")
	val beta:Symbol = Symbol("beta")
	val gamma:Symbol = Symbol("gamma")
	val delta:Symbol = Symbol("delta")
	val epsi:Symbol = Symbol("epsi")
	val epsiv:Symbol = Symbol("epsiv")
	val zeta:Symbol = Symbol("zeta")
	val eta:Symbol = Symbol("eta")
	val theta:Symbol = Symbol("theta")
	val thetav:Symbol = Symbol("thetav")
	val iota:Symbol = Symbol("iota")
	val kappa:Symbol = Symbol("kappa")
	val lambda:Symbol = Symbol("lambda")
	val mu:Symbol = Symbol("mu")
	val nu:Symbol = Symbol("nu")
	val xi:Symbol = Symbol("xi")
	val pi:Symbol = Symbol("pi")
	val piv:Symbol = Symbol("piv")
	val rho:Symbol = Symbol("rho")
	val rhov:Symbol = Symbol("rhov")
	val sigma:Symbol = Symbol("sigma")
	val sigmav:Symbol = Symbol("sigmav")
	val isin:Symbol = Symbol("isin")
	val tau:Symbol = Symbol("tau")
	val upsi:Symbol = Symbol("upsi")
	val phi:Symbol = Symbol("phi")
	val phiv:Symbol = Symbol("phiv")
	val chi:Symbol = Symbol("chi")
	val psi:Symbol = Symbol("psi")
	val omega:Symbol = Symbol("omega")
	
	val Gamma:Symbol = Symbol("Gamma")
	val Delta:Symbol = Symbol("Delta")
	val Theta:Symbol = Symbol("Theta")
	val Lambda:Symbol = Symbol("Lambda")
	val Xi:Symbol = Symbol("Xi")
	val Pi:Symbol = Symbol("Pi")
	val Sigma:Symbol = Symbol("Sigma")
	val Upsi:Symbol = Symbol("Upsi")
	val Phi:Symbol = Symbol("Phi")
	val Psi:Symbol = Symbol("Psi")
	val Omega:Symbol = Symbol("Omega")
	val nabla:Symbol = Symbol("nabla")
	
	val mi = mu
	val ni = nu
	
	val eul:Symbol = Symbol("eul")
	
	lazy val BasicSymbols2PlainTextMap:Map[Symbol,String] = Map(
			alpha 	-> "α",
			beta 	-> "β",
			gamma 	-> "γ",
			delta 	-> "δ",
			epsi 	-> "ϵ",
			epsiv 	-> "ε",
			zeta 	-> "ζ",
			eta 	-> "η",
			theta 	-> "θ",
			thetav 	-> "ϑ",
			iota 	-> "ι",
			kappa 	-> "κ",
			lambda 	-> "λ",
			mu 		-> "μ",
			nu 		-> "ν",
			xi 		-> "ξ",
			pi 		-> "π",
			piv 	-> "ϖ",
			rho 	-> "ρ",
			rhov 	-> "ϱ",
			sigma 	-> "σ",
			sigmav 	-> "ς",
			isin 	-> "isin",
			tau 	-> "τ",
			upsi 	-> "υ",
			phi 	-> "ϕ",
			phiv 	-> "φ",
			chi 	-> "χ",
			psi 	-> "ψ",
			omega 	-> "ω",
			Gamma 	-> "Γ",
			Delta 	-> "Δ",
			Theta 	-> "Θ",
			Lambda 	-> "Λ",
			Xi 		-> "Ξ",
			Pi 		-> "Π",
			Sigma 	-> "Σ",
			Upsi 	-> "ϒ",
			Phi 	-> "Φ",
			Psi 	-> "Ψ",
			Omega 	-> "Ω",
			nabla 	-> "nabla",
			eul		-> "e"
	)
	
	lazy val String2PlainTextMap:Map[String,String] = BasicSymbols2PlainTextMap.map(a => (a._1.name,a._2))
	
	def toPlainText(s:String):String = String2PlainTextMap.get(s).getOrElse(s)
	
	lazy val BasicSymbols2MathMLMap:Map[Symbol,String] = Map(
			alpha 	-> "&alpha;",
			beta 	-> "&beta;",
			gamma 	-> "&gamma;",
			delta 	-> "&delta;",
			epsi 	-> "&epsi;",
			epsiv 	-> "&epsiv;",
			zeta 	-> "&zeta;",
			eta 	-> "&eta;",
			theta 	-> "&theta;",
			thetav 	-> "&thetav;",
			iota 	-> "&iota;",
			kappa 	-> "&kappa;",
			lambda 	-> "&lambda;",
			mu 		-> "&mu;",
			nu 		-> "&nu;",
			xi 		-> "&xi;",
			pi 		-> "&pi;",
			piv 	-> "&piv;",
			rho 	-> "&rho;",
			rhov 	-> "&rhov;",
			sigma 	-> "&sigma;",
			sigmav 	-> "&sigmav;",
			isin 	-> "&isin;",
			tau 	-> "&tau;",
			upsi 	-> "&upsi;",
			phi 	-> "&phi;",
			phiv 	-> "&phiv;",
			chi 	-> "&chi;",
			psi 	-> "&psi;",
			omega 	-> "&omega;",
			Gamma 	-> "&Gamma;",
			Delta 	-> "&Delta;",
			Theta 	-> "&Theta;",
			Lambda 	-> "&Lambda;",
			Xi 		-> "&Xi;",
			Pi 		-> "&Pi;",
			Sigma 	-> "&Sigma;",
			Upsi 	-> "&Upsi;",
			Phi 	-> "&Phi;",
			Psi 	-> "&Psi;",
			Omega 	-> "&Omega;",
			nabla 	-> "&nabla;",
			eul		-> "e"
	)
	
	lazy val String2MathMLMap:Map[String,String] = BasicSymbols2MathMLMap.map(a => (a._1.name,a._2))
	
	def toMathML(s:String):String = String2MathMLMap.get(s).getOrElse(s)

}

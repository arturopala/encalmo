package org.encalmo.expression

/**
 * Basic latin symbols set
 * @author artur.opala
 */
object BasicSymbols {
	
	lazy val a:Symbol = Symbol("a")
  	lazy val b:Symbol = Symbol("b")
	lazy val c:Symbol = Symbol("c")
	lazy val d:Symbol = Symbol("d")
	lazy val e:Symbol = Symbol("e")
	lazy val f:Symbol = Symbol("f")
	lazy val g:Symbol = Symbol("g")
	lazy val h:Symbol = Symbol("h")
	lazy val i:Symbol = Symbol("i")
	lazy val j:Symbol = Symbol("j")
  	lazy val k:Symbol = Symbol("k")
	lazy val l:Symbol = Symbol("l")
	lazy val m:Symbol = Symbol("m")
	lazy val n:Symbol = Symbol("n")
	lazy val o:Symbol = Symbol("o")
	lazy val p:Symbol = Symbol("p")
	lazy val q:Symbol = Symbol("q")
	lazy val r:Symbol = Symbol("r")
	lazy val s:Symbol = Symbol("s")
	lazy val t:Symbol = Symbol("t")
	lazy val u:Symbol = Symbol("u")
	lazy val v:Symbol = Symbol("v")
	lazy val w:Symbol = Symbol("w")
	lazy val x:Symbol = Symbol("x")
	lazy val y:Symbol = Symbol("y")
	lazy val z:Symbol = Symbol("z")
	
	lazy val A:Symbol = Symbol("A")
  	lazy val B:Symbol = Symbol("B")
	lazy val C:Symbol = Symbol("C")
	lazy val D:Symbol = Symbol("D")
	lazy val E:Symbol = Symbol("E")
	lazy val F:Symbol = Symbol("F")
	lazy val G:Symbol = Symbol("G")
	lazy val H:Symbol = Symbol("H")
	lazy val I:Symbol = Symbol("I")
	lazy val J:Symbol = Symbol("J")
  	lazy val K:Symbol = Symbol("K")
	lazy val L:Symbol = Symbol("L")
	lazy val M:Symbol = Symbol("M")
	lazy val N:Symbol = Symbol("N")
	lazy val O:Symbol = Symbol("O")
	lazy val P:Symbol = Symbol("P")
	lazy val Q:Symbol = Symbol("Q")
	lazy val R:Symbol = Symbol("R")
	lazy val S:Symbol = Symbol("S")
	lazy val T:Symbol = Symbol("T")
	lazy val U:Symbol = Symbol("U")
	lazy val V:Symbol = Symbol("V")
	lazy val W:Symbol = Symbol("W")
	lazy val X:Symbol = Symbol("X")
	lazy val Y:Symbol = Symbol("Y")
	lazy val Z:Symbol = Symbol("Z")
	
	lazy val alpha:Symbol = Symbol("alpha")
	lazy val beta:Symbol = Symbol("beta")
	lazy val gamma:Symbol = Symbol("gamma")
	lazy val delta:Symbol = Symbol("delta")
	lazy val epsi:Symbol = Symbol("epsi")
	lazy val epsiv:Symbol = Symbol("epsiv")
	lazy val zeta:Symbol = Symbol("zeta")
	lazy val eta:Symbol = Symbol("eta")
	lazy val theta:Symbol = Symbol("theta")
	lazy val thetav:Symbol = Symbol("thetav")
	lazy val iota:Symbol = Symbol("iota")
	lazy val kappa:Symbol = Symbol("kappa")
	lazy val lambda:Symbol = Symbol("lambda")
	lazy val mu:Symbol = Symbol("mu")
	lazy val nu:Symbol = Symbol("nu")
	lazy val xi:Symbol = Symbol("xi")
	lazy val pi:Symbol = Symbol("pi")
	lazy val piv:Symbol = Symbol("piv")
	lazy val rho:Symbol = Symbol("rho")
	lazy val rhov:Symbol = Symbol("rhov")
	lazy val sigma:Symbol = Symbol("sigma")
	lazy val sigmav:Symbol = Symbol("sigmav")
	lazy val isin:Symbol = Symbol("isin")
	lazy val tau:Symbol = Symbol("tau")
	lazy val upsi:Symbol = Symbol("upsi")
	lazy val phi:Symbol = Symbol("phi")
	lazy val phiv:Symbol = Symbol("phiv")
	lazy val chi:Symbol = Symbol("chi")
	lazy val psi:Symbol = Symbol("psi")
	lazy val omega:Symbol = Symbol("omega")
	
	lazy val Gamma:Symbol = Symbol("Gamma")
	lazy val Delta:Symbol = Symbol("Delta")
	lazy val Theta:Symbol = Symbol("Theta")
	lazy val Lambda:Symbol = Symbol("Lambda")
	lazy val Xi:Symbol = Symbol("Xi")
	lazy val Pi:Symbol = Symbol("Pi")
	lazy val Sigma:Symbol = Symbol("Sigma")
	lazy val Upsi:Symbol = Symbol("Upsi")
	lazy val Phi:Symbol = Symbol("Phi")
	lazy val Psi:Symbol = Symbol("Psi")
	lazy val Omega:Symbol = Symbol("Omega")
	lazy val nabla:Symbol = Symbol("nabla")
	
	lazy val mi = mu
	lazy val ni = nu
	
	lazy val eul:Symbol = Symbol("eul")
	
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

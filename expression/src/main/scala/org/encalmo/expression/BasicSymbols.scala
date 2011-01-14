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
	
	val alpha:Symbol = Symbol("α")
	val beta:Symbol = Symbol("β")
	val gamma:Symbol = Symbol("γ")
	val delta:Symbol = Symbol("δ")
	val epsi:Symbol = Symbol("ϵ")
	val epsiv:Symbol = Symbol("ε")
	val zeta:Symbol = Symbol("ζ")
	val eta:Symbol = Symbol("η")
	val theta:Symbol = Symbol("θ")
	val thetav:Symbol = Symbol("ϑ")
	val iota:Symbol = Symbol("ι")
	val kappa:Symbol = Symbol("κ")
	val lambda:Symbol = Symbol("λ")
	val mu:Symbol = Symbol("μ")
	val nu:Symbol = Symbol("ν")
	val xi:Symbol = Symbol("ξ")
	val pi:Symbol = Symbol("π")
	val piv:Symbol = Symbol("ϖ")
	val rho:Symbol = Symbol("ρ")
	val rhov:Symbol = Symbol("ϱ")
	val sigma:Symbol = Symbol("σ")
	val sigmav:Symbol = Symbol("ς")
	val isin:Symbol = Symbol("isin")
	val tau:Symbol = Symbol("τ")
	val upsi:Symbol = Symbol("υ")
	val phi:Symbol = Symbol("ϕ")
	val phiv:Symbol = Symbol("φ")
	val chi:Symbol = Symbol("χ")
	val psi:Symbol = Symbol("ψ")
	val omega:Symbol = Symbol("ω")
	
	val Gamma:Symbol = Symbol("Γ")
	val Delta:Symbol = Symbol("Δ")
	val Theta:Symbol = Symbol("Θ")
	val Lambda:Symbol = Symbol("Λ")
	val Xi:Symbol = Symbol("Ξ")
	val Pi:Symbol = Symbol("Π")
	val Sigma:Symbol = Symbol("Σ")
	val Upsi:Symbol = Symbol("ϒ")
	val Phi:Symbol = Symbol("Φ")
	val Psi:Symbol = Symbol("Ψ")
	val Omega:Symbol = Symbol("Ω")
	val nabla:Symbol = Symbol("nabla")
	
	val mi = mu
	val ni = nu
	
	val eul:Symbol = Symbol("e")
	
	val BasicSymbols2MathMLMap:Map[Symbol,String] = Map(
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
			nabla 	-> "&nabla;"
	)
	
	val String2MathMLMap:Map[String,String] = BasicSymbols2MathMLMap.map(a => (a._1.name,a._2))
	
	def toMathML(s:String):String = String2MathMLMap.get(s).getOrElse(s)

}

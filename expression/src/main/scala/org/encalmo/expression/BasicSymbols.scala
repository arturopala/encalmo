package org.encalmo.expression

/**
 * Basic latin and greek symbols set
 * @author artur.opala
 */
trait BasicSymbols extends SymbolConfigurator {
	
	val a:Symbol = symbol("a")
  	val b:Symbol = symbol("b")
	val c:Symbol = symbol("c")
	val d:Symbol = symbol("d")
	val e:Symbol = symbol("e")
	val f:Symbol = symbol("f")
	val g:Symbol = symbol("g")
	val h:Symbol = symbol("h")
	val i:Symbol = symbol("i")
	val j:Symbol = symbol("j")
  	val k:Symbol = symbol("k")
	val l:Symbol = symbol("l")
	val m:Symbol = symbol("m")
	val n:Symbol = symbol("n")
	val o:Symbol = symbol("o")
	val p:Symbol = symbol("p")
	val q:Symbol = symbol("q")
	val r:Symbol = symbol("r")
	val s:Symbol = symbol("s")
	val t:Symbol = symbol("t")
	val u:Symbol = symbol("u")
	val v:Symbol = symbol("v")
	val w:Symbol = symbol("w")
	val x:Symbol = symbol("x")
	val y:Symbol = symbol("y")
	val z:Symbol = symbol("z")
	
	val A:Symbol = symbol("A")
  	val B:Symbol = symbol("B")
	val C:Symbol = symbol("C")
	val D:Symbol = symbol("D")
	val E:Symbol = symbol("E")
	val F:Symbol = symbol("F")
	val G:Symbol = symbol("G")
	val H:Symbol = symbol("H")
	val I:Symbol = symbol("I")
	val J:Symbol = symbol("J")
  	val K:Symbol = symbol("K")
	val L:Symbol = symbol("L")
	val M:Symbol = symbol("M")
	val N:Symbol = symbol("N")
	val O:Symbol = symbol("O")
	val P:Symbol = symbol("P")
	val Q:Symbol = symbol("Q")
	val R:Symbol = symbol("R")
	val S:Symbol = symbol("S")
	val T:Symbol = symbol("T")
	val U:Symbol = symbol("U")
	val V:Symbol = symbol("V")
	val W:Symbol = symbol("W")
	val X:Symbol = symbol("X")
	val Y:Symbol = symbol("Y")
	val Z:Symbol = symbol("Z")
	
	val ls:Symbol = symbol("lscr")
	
	val alpha:Symbol = symbol("alpha")
	val beta:Symbol = symbol("beta")
	val gamma:Symbol = symbol("gamma")
	val delta:Symbol = symbol("delta")
	val epsi:Symbol = symbol("epsi")
	val epsiv:Symbol = symbol("epsiv")
	val zeta:Symbol = symbol("zeta")
	val eta:Symbol = symbol("eta")
	val theta:Symbol = symbol("theta")
	val thetav:Symbol = symbol("thetav")
	val iota:Symbol = symbol("iota")
	val kappa:Symbol = symbol("kappa")
	val lambda:Symbol = symbol("lambda")
	val mu:Symbol = symbol("mu")
	val nu:Symbol = symbol("nu")
	val xi:Symbol = symbol("xi")
	val pi:Symbol = symbol("pi")
	val piv:Symbol = symbol("piv")
	val rho:Symbol = symbol("rho")
	val rhov:Symbol = symbol("rhov")
	val sigma:Symbol = symbol("sigma")
	val sigmav:Symbol = symbol("sigmav")
	val isin:Symbol = symbol("isin")
	val tau:Symbol = symbol("tau")
	val upsi:Symbol = symbol("upsi")
	val phi:Symbol = symbol("phi")
	val phiv:Symbol = symbol("phiv")
	val chi:Symbol = symbol("chi")
	val psi:Symbol = symbol("psi")
	val omega:Symbol = symbol("omega")
	
	val Gamma:Symbol = symbol("Gamma")
	val Delta:Symbol = symbol("Delta")
	val Theta:Symbol = symbol("Theta")
	val Lambda:Symbol = symbol("Lambda")
	val Xi:Symbol = symbol("Xi")
	val Pi:Symbol = symbol("Pi")
	val Sigma:Symbol = symbol("Sigma")
	val Upsi:Symbol = symbol("Upsi")
	val Phi:Symbol = symbol("Phi")
	val Psi:Symbol = symbol("Psi")
	val Omega:Symbol = symbol("Omega")
	val nabla:Symbol = symbol("nabla")
	
	val diam:Symbol = symbol("⌀")

    val mi = mu
	val ni = nu

}

object BasicSymbols extends BasicSymbols {

    val id: String = "global"
    val dictionary = None

    val eul:Symbol = symbol("eul")
    val grav:Symbol = symbol("grav")

    def toPlainText(symbol: Symbol):String = String2PlainTextMap.get(symbol.name).getOrElse(symbol.name)
    def toMathML(symbol: Symbol):String = String2MathMLMap.get(symbol.name).getOrElse(symbol.name)

    lazy val BasicSymbols2PlainTextMap:Map[String,String] = Map(
        "alpha" 	-> "α",
        "beta" 	-> "β",
        "gamma" 	-> "γ",
        "delta" 	-> "δ",
        "epsi" 	-> "ϵ",
        "epsiv" 	-> "ε",
        "zeta" 	-> "ζ",
        "eta" 	-> "η",
        "theta" 	-> "θ",
        "thetav" 	-> "ϑ",
        "iota" -> "ι",
        "kappa" -> "κ",
        "lambda" -> "λ",
        "mu" -> "μ",
        "nu" -> "ν",
        "xi" -> "ξ",
        "pi" -> "π",
        "piv" -> "ϖ",
        "rho" -> "ρ",
        "rhov" -> "ϱ",
        "sigma" -> "σ",
        "sigmav" -> "ς",
        "isin" -> "isin",
        "tau" -> "τ",
        "upsi" -> "υ",
        "phi" -> "ϕ",
        "phiv" -> "φ",
        "chi" -> "χ",
        "psi" -> "ψ",
        "omega" -> "ω",
        "Gamma" -> "Γ",
        "Delta" -> "Δ",
        "Theta" -> "Θ",
        "Lambda" -> "Λ",
        "Xi" -> "Ξ",
        "Pi" -> "Π",
        "Sigma" -> "Σ",
        "Upsi" -> "ϒ",
        "Phi" -> "Φ",
        "Psi" -> "Ψ",
        "Omega" -> "Ω",
        "nabla" -> "nabla",
        "eul" -> "e",
        "grav" -> "g",
        "ls" -> "l"
    )

    lazy val String2PlainTextMap:Map[String,String] = BasicSymbols2PlainTextMap.map(a => (a._1.name,a._2))

    lazy val BasicSymbols2MathMLMap:Map[String,String] = Map(
        "alpha" -> "&alpha;",
        "beta" -> "&beta;",
        "gamma" -> "&gamma;",
        "delta" -> "&delta;",
        "epsi" -> "&epsi;",
        "epsiv" -> "&epsiv;",
        "zeta" -> "&zeta;",
        "eta" -> "&eta;",
        "theta" -> "&theta;",
        "thetav" -> "&thetav;",
        "iota" -> "&iota;",
        "kappa" -> "&kappa;",
        "lambda" -> "&lambda;",
        "mu" -> "&mu;",
        "nu" -> "&nu;",
        "xi" -> "&xi;",
        "pi" -> "&pi;",
        "piv" -> "&piv;",
        "rho" -> "&rho;",
        "rhov" -> "&rhov;",
        "sigma" -> "&sigma;",
        "sigmav" -> "&sigmav;",
        "isin" -> "&isin;",
        "tau" -> "&tau;",
        "upsi" -> "&upsi;",
        "phi" -> "&phi;",
        "phiv" -> "&phiv;",
        "chi" -> "&chi;",
        "psi" -> "&psi;",
        "omega" -> "&omega;",
        "Gamma" -> "&Gamma;",
        "Delta" -> "&Delta;",
        "Theta" -> "&Theta;",
        "Lambda" -> "&Lambda;",
        "Xi" -> "&Xi;",
        "Pi" -> "&Pi;",
        "Sigma" -> "&Sigma;",
        "Upsi" -> "&Upsi;",
        "Phi" -> "&Phi;",
        "Psi" -> "&Psi;",
        "Omega" -> "&Omega;",
        "nabla" -> "&nabla;",
        "eul" -> "&escr;",
        "grav" -> "&gscr;",
        "ls" -> "&lscr;"
    )

    lazy val String2MathMLMap:Map[String,String] = BasicSymbols2MathMLMap.map(a => (a._1.name,a._2))
    
}

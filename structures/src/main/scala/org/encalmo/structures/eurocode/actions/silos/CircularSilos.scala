package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.calculation._
import org.encalmo.structures.eurocode.steel.SteelSymbols


/** Circular silos symbols */
object CircularSiloSymbols extends SymbolConfigurator {

    val dictionary, contextId = "circularSilos"

    //input geometry
    val d1 = symbol(BasicSymbols.d | 1) unit SI.mm acc 1
    //Średnica zewnętrzna silosu
    val h1 = symbol(BasicSymbols.h | 1) unit SI.mm acc 1
    //Wysokość komory silosu
    val h2 = symbol(BasicSymbols.h | 2) unit SI.mm acc 1
    //Wysokość leja silosu
    val de = symbol(BasicSymbols.d | BasicSymbols.e) unit SI.mm acc 1
    //Średnica otworu wysypowego leja
    val h3 = symbol(BasicSymbols.h | 3) unit SI.mm acc 1
    //Prześwit pomiędzy lejem a poziomem terenu
    val h4 = symbol(BasicSymbols.h | 4) unit SI.mm acc 1
    //Wysokość całkowita silosu
    val r = symbol(BasicSymbols.r) unit "mm" acc 1 //Promień powłoki silosu
}

/** Thin walled circular silos with steep hopper expressions */
object ThinWalledCircularSlenderSiloWithSteepHopperExpressions extends MapContext {

    import SteelSymbols.{gammas, E, fy, gammaM1}
    import SiloSymbols._
    import CircularSiloSymbols._
    import ParticulateSolidSymbols._

    //calculated geometry
    dc := d1 - 2 * t
    r := (d1 - t) / 2
    A := PI * (dc ^ 2) / 4
    U := PI * dc
    AU := A / U
    beta := arctan((dc - de) / (2 * h2))
    hh := (dc / 2) / tan(beta)
    he := (de / 2) / tan(beta)
    htp := (dc * tan(fir)) / 2
    ho := htp / 2
    hc := h1 - ho
    hb := hc + hh
    hcdc := hc / dc
    tr := t
    h4 := h3 + h2 + h1

    //volumes
    Vc := A * h1
    Vh := 1 / 3d * PI * h2 * (((dc / 2) ^ 2) + (dc / 2) * (de / 2) + ((de / 2) ^ 2))
    V := Vc + Vh
    W := V * gammau / GRAV
    Sc := U * h1
    Sh := PI * (h2 / cos(beta)) * (dc + de)
    S := Sc + Sh

    //filling symmetrical load
    //  (1) Maximum normal pressure on vertical wall
    zo(1) := 1 / (K_u * mu_l) * AU
    YJ(1) := 1 - (EUL ^ (-z / zo(1)))
    pho(1) := (gammau / mu_l) * AU
    phf(1) := fx(pho(1) * YJ(1), z)
    phf1 := EvalAt(phf(1), z -> hc / 4)
    phf2 := EvalAt(phf(1), z -> hc / 2)
    phf3 := EvalAt(phf(1), z -> 0.75 * hc)
    phft := EvalAt(phf(1), z -> hc)
    //  (2) Maximum frictional traction on vertical wall
    zo(2) := 1 / (K_u * mu_u) * AU
    YJ(2) := 1 - (EUL ^ (-z / zo(2)))
    pho(2) := (gammau / mu_u) * AU
    phf(2) := fx(pho(2) * YJ(2), z)
    pwf := mu_u * phf(2)
    nfzSk := mu_u * pho(2) * (z - zo(2) * YJ(2))
    nfzSkt := EvalAt(nfzSk, z -> hc)
    //  (3) Maximum vertical load on hopper or silo bottom
    Cb := 1.2
    zo(3) := 1 / (K_l * mu_l) * AU
    YJ(3) := 1 - (EUL ^ (-z / zo(3)))
    pho(3) := (gammau / mu_l) * AU
    pvf := (pho(3) / K_l) * YJ(3)
    pvft := EvalAt(Cb * pvf, z -> hc)

    //filling patch load
    ef := dc / 4
    Ef := 2 * ef / dc
    Cpf := 0.21 * Cop * (1 + 2 * (Ef ^ 2)) * (1 - (EUL ^ (-1.5 * (hcdc - 1))))
    ppf := Cpf * phf(1)
    zp := min(zo(1), hc / 2)
    ppfzp := EvalAt(ppf, z -> zp)
    s := (PI * dc) / 16
    Fpf1 := (PI / 2) * s * dc * ppfzp

    //discharge symmetrical load
    Ch := 1.15
    Cw := 1.1
    phe := Ch * phf(1)
    pwe := Cw * pwf
    phet := EvalAt(phe, z -> hc)
    nezSk := Cw * mu_u * pho(2) * (z - zo(2) * YJ(2))
    nezSkt := EvalAt(nezSk, z -> hc)

    //discharge patch load
    Cpe := 0.42 * Cop * (1 + 2 * (Ef ^ 2)) * (1 - (EUL ^ (-1.5 * (hcdc - 1))))
    ppe := Cpe * phe
    ppezp := EvalAt(ppe, z -> zp)
    Fpe1 := (PI / 2) * s * dc * ppezp

    //Loads on silo hoppers and silo bottoms

    //filling load on hopper
    muheff := mu_l
    Ff := 1 - (0.2 / (1 + (tan(beta) / mu_l)))
    nh := 2 * (1 - 0.2) * muheff * cot(beta)
    pv := fx(((gammau * hh) / (nh - 1)) * ((x / hh) - ((x / hh) ^ nh)) + pvft * ((x / hh) ^ nh), x)
    pnf := Ff * pv
    ptf := mu_l * Ff * pv
    pnf0 := EvalAt(pnf, x -> he)
    ptf0 := EvalAt(ptf, x -> he)
    pnf1 := EvalAt(pnf, x -> hh)
    ptf1 := EvalAt(ptf, x -> hh)

    //discharge load on hopper
    fiwh := arctan(mu_l)
    epsilon := fiwh + arcsin(sin(fiwh) / sin(fiim))
    Fe := (1 + sin(fiim) * cos(epsilon)) / (1 - sin(fiim) * cos(beta + epsilon))
    pne := Fe * pv
    pte := mu_l * Fe * pv
    pne0 := EvalAt(pne, x -> he)
    pte0 := EvalAt(pte, x -> he)
    pne1 := EvalAt(pne, x -> hh)
    pte1 := EvalAt(pte, x -> hh)

    //ciezar wlasny
    Grk := gammas * tr * A / cos(alpha)
    Gck := gammas * t * U
    Ghk := gammas * th * Sh
    Gpk := 10 unit SI.kN
    //Gk := Gck+Ghk+Grk+Gpk

    Quk := (2.5 unit SI.kN / SI.m2) * A

    //obciazenie sniegiem
    sk := max(0.7, (0.007 * HM - 1.4).nounit)
    mi1 := 0.8
    Ce := 0.8
    Ct := 1.0
    sr := mi1 * Ce * Ct * sk
    Qsk := sr * A / U

    //obciazenie wiatrem
    vbo := 22
    qbo := 0.30
    coz := 1.0
    crz := 1.0 * ((z / (10 unit SI.m)) ^ 0.17)
    cez := 2.3 * ((z / (10 unit SI.m)) ^ 0.24)
    vmz := crz * coz * vbo
    qpz := cez * qbo
    cscd := 1.0
    cpi := -0.6
    ze := h4
    Re := d1 * EvalAt(vmz, z -> ze) / ((15 * 1E-6) unit SI.m2 / SI.s)
    Aref := d1 * (h4 - h3)
    cf := 1.2 + ((0.18 * log(10 * ((200 unit SI.m) / d1))) / (1 + 0.4 * log(Re / 1E5)))
    Fw := cscd * cf * EvalAt(qpz, z -> ze) * Aref
    wemax := 1.0 * EvalAt(qpz, z -> ze)
    wi := cpi * EvalAt(qpz, z -> ze)

    omega := h1 / sqrt(r * t)
    Cx := 1.0
    sigxRcr := 0.605 * Cx * E * (t / r)
    Qx := 25
    dwk := t / Qx * sqrt(r / t)
    alphax := 0.62 / (1 + 1.91 * ((dwk / t) ^ 1.44))
    betax := 0.6
    etax := 1
    lambdax0 := 0.2
    lambdapx := sqrt(alphax / (1 - betax))
    lambdax := sqrt(fy / sigxRcr)
    chix := rangeChoiceLE(lambdax, 1 - betax * (((lambdax - lambdax0) / (lambdapx - lambdax0)) ^ etax), lambdapx, alphax / (lambdax ^ 2))
    sigxRd := fy * chix / gammaM1

    Cteta := 1.0
    sigtRcr := 0.92 * E * (Cteta / omega) * (t / r)
    alphat := 0.65
    betat := 0.6
    etat := 1
    lambdat0 := 0.4
    lambdapt := sqrt(alphat / (1 - betat))
    lambdat := sqrt(fy / sigtRcr)
    chiteta := rangeChoiceLE(lambdat, 1 - betat * (((lambdat - lambdat0) / (lambdapt - lambdat0)) ^ etax), lambdapt, alphat / (lambdat ^ 2))
    sigtRd := fy * chiteta / gammaM1
    kw := 0.46 * (1 + 0.1 * sqrt((Cteta / omega) * (t / r)))

    Ctau := 1.0
    tauxtRcr := 0.75 * E * Ctau * sqrt(1 / omega) * ((t / r) ^ 1.25)
    alphatau := 0.65
    betatau := 0.6
    etatau := 1
    lambdatau0 := 0.4
    lambdaptau := sqrt(alphatau / (1 - betatau))
    lambdatau := sqrt(fy / (sqrt(3) * tauxtRcr))
    chitau := rangeChoiceLE(lambdatau, 1 - betatau * (((lambdatau - lambdatau0) / (lambdaptau - lambdatau0)) ^ etax), lambdaptau, alphatau / (lambdatau ^ 2))
    tauRd := chitau * fy / sqrt(3)

    lR := min(0.1 * h1, 0.16 * r * sqrt(r / t))
    lf := h1 - 2 * lR
    qwzd := (wemax * kw - wi) * 1.5
    val NxEd1 = NxEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
    NxEd(1) := (Gck * (h1 - lR) + Grk + Gpk) * 1.35 + Quk * 1.5
    MwEd := qwzd * d1 * (((h1 - lR) ^ 2) / 2)
    val sigmaxEd1 = sigmaxEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
    sigmaxEd1 := MwEd / (PI * t * (r ^ 2)) + NxEd(1) / (2 * PI * r * t)
    val sigmatEd1 = sigmatEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej, wg wzoru z [1993-1-6] D.30, "
    sigmatEd1 := qwzd * (r / t)
    val QtauEd1 = QtauEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
    QtauEd1 := qwzd * d1 * (h1 - lR)
    val tauEd1 = tauEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
    tauEd1 := QtauEd1 / (PI * r * t)
    val zeta1 = zeta(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
    kx := 1.25 + 0.75 * chix
    kt := 1.25 + 0.75 * chiteta
    ktau := 1.25 + 0.25 * chitau
    ki := (chix * chiteta) ^ 2
    zeta1 := ((sigmaxEd1 / sigxRd) ^ kx) - (ki * (sigmaxEd1 / sigxRd) * (sigmatEd1 / sigtRd)) + ((sigmatEd1 / sigtRd) ^ kt) + ((tauEd1 / tauRd) ^ ktau)

    val NxEd2 = NxEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    NxEd(2) := (Gck * (h1 - lR) + Grk + Gpk) * 1.35 + Quk * 1.5 + Qsk * U * 1.5 + EvalAt(nezSk, z -> (hc - lR)) * U * 1.5
    MeEd := Fpe1 * (hc - zp - lR)
    val sigmaxEd2 = sigmaxEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    sigmaxEd2 := MeEd / (PI * t * (r ^ 2)) + NxEd(2) / (2 * PI * r * t)
    val sigmatEd2 = sigmatEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej, wg wzoru z [1993-1-6] D.30, "
    sigmatEd2 := EvalAt(phe, z -> zp) * (r / t)
    val QtauEd2 = QtauEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    QtauEd2 := Fpe1
    val tauEd2 = tauEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    tauEd2 := QtauEd2 / (PI * r * t)
    val zeta2 = zeta(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    zeta2 := ((sigmaxEd2 / sigxRd) ^ kx) - (ki * (sigmaxEd2 / sigxRd) * (sigmatEd2 / sigtRd)) + ((sigmatEd2 / sigtRd) ^ kt) + ((tauEd2 / tauRd) ^ ktau)

    s2 := s1 + 2 * hpp
    N1 := ((Gck * h1 + Grk + Gpk + Ghk) * 1.35 + (Quk + Qsk * U + V * gammau) * 1.5) / ns
    sigmax1 := N1 / (s1 * tpp)
    sigmax2 := N1 / (s2 * tp)
    sigmax1cr := 0.605 * (1 + 1.5 * ((r / hpp) ^ 2) * (tpp / r)) * E * (tpp / r)
    lambdax1 := sqrt(fy / sigmax1cr)
    chix1 := rangeChoiceLELE(lambdax1, 1, lambdax0, 1 - betax * (((lambdax1 - lambdax0) / (lambdapx - lambdax0)) ^ etax), lambdapx, alphax / (lambdax1 ^ 2))
    sigxRd1 := fy * chix1 / gammaM1

    // end of context initialization
    lock()

}

/** Circular slender silos calculation */
class ThinWalledCircularSlenderSiloWithSteepHopper(
       diameter: Expression,
       heightOfChamber: Expression,
       heightOfHopper: Expression,
       thicknessOfChamberWall: Expression,
       thicknessOfHopperWall: Expression,
       thicknessOfRing: Expression,
       heightOfRing: Expression,
       widthOfColumn: Expression,
       numberOfColumns: Expression,
       diameterOfOutlet: Expression,
       particulateSolid: ParticulateSolid,
       wallType: Expression,
       steel: Context
    )
    extends Calculation {

    import SteelSymbols.{E, fy, gammaM1}
    import SiloSymbols._
    import CircularSiloSymbols._
    import ParticulateSolidSymbols._
    import ThinWalledCircularSlenderSiloWithSteepHopperExpressions._

    acc(0.01)

    particulateSolid(ParticulateSolidSymbols.D) = wallType

    this add ThinWalledCircularSlenderSiloWithSteepHopperExpressions
    this add particulateSolid
    this add steel

    d1 := diameter
    h1 := heightOfChamber
    h2 := heightOfHopper
    t := thicknessOfChamberWall
    th := thicknessOfHopperWall
    de := diameterOfOutlet
    h3 := 2.5 unit SI.m
    alpha := 5 unit SI.deg
    HM := 140 unit SI.m
    s1 := widthOfColumn
    hpp := heightOfRing
    tp := t
    tpp := thicknessOfRing
    ns := numberOfColumns
    gammaM1 := 1.1

    //input geometry
    def inputGeometry = NumSection(TextToTranslate("_inputGeometry", SiloSymbols.dictionary),
        Evaluate(d1, h1, h2, de, t, th, tr, tp, tpp, alpha, h3, h4)
    )

    //input assertions

    //calculated geometry
    def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry", SiloSymbols.dictionary),
        Evaluate(r, dc, A, U, AU, beta, hh, he, htp, ho, hc, hb, hcdc, Sc, Sh, S),
        AssertionL("[1991-4] 1.1.2 (3)", this, hb / dc, 10),
        AssertionL("[1991-4] 1.1.2 (3)", this, hb, 100 unit SI.m),
        AssertionL("[1991-4] 1.1.2 (3)", this, dc, 60 unit SI.m),
        AssertionGE("[1991-4] 5.1 (2)", this, hc / dc, 2)
    )

    //volumes
    def volumes = NumSection(TextToTranslate("_volumes", SiloSymbols.dictionary),
        Evaluate(Vc, Vh, V, W)
    )

    def ciezarWlasny = NumSection("Oddziaływania od ciężaru własnego", Evaluate(Gck, Ghk, Grk, Gpk))

    def obciazenieUzytkowe = NumSection("Obciążenie użytkowe", Evaluate(Quk))

    def obciazenieSniegiem = NumSection("Oddziaływania od obciążenia śniegiem", Evaluate(HM, sk, mi1, Ce, Ct, sr, Qsk))

    def obciazenieWiatrem = NumSection("Oddziaływania od obciążenia wiatrem", Evaluate(qbo, cez, qpz, cscd, ze, Re, Aref, cf, Fw, wemax, cpi, wi))

    //filling symmetrical load
    def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad", SiloSymbols.dictionary), "[1991-4] 5.2.1.1",
        NumSection(TextToTranslate("_fillingSymmetricalLoad_1", SiloSymbols.dictionary),
            Evaluate(zo(1), pho(1), YJ(1), phf(1), phf1, phf2, phf3, phft)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_2", SiloSymbols.dictionary),
            Evaluate(zo(2), pho(2), YJ(2), phf(2), pwf, nfzSk, nfzSkt)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_3", SiloSymbols.dictionary),
            Evaluate(zo(3), pho(3), YJ(3), pvf)
        )
    )

    //filling patch load
    def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad", SiloSymbols.dictionary), "[1991-4] 5.2.1.2, 5.2.1.4",
        Evaluate(ef, Ef, Cpf, ppf, zp, ppfzp, s, Fpf1)
    )

    //discharge symmetrical load
    def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad", SiloSymbols.dictionary), "[1991-4] 5.2.2.1",
        Evaluate(Ch, Cw, phe, phet, pwe, nezSk, nezSkt)
    )

    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad", SiloSymbols.dictionary), "[1991-4] 5.2.2.2, 5.2.2.4",
        Evaluate(Cpe, ppe, zp, ppezp, Fpe1)
    )

    //filling loads on silo hoppers
    def fillingHopperLoad = NumSection(TextToTranslate("_fillingHopperLoad", SiloSymbols.dictionary), "[1991-4] 6.1.2, 6.3.2",
        AssertionL("leja stromego [1991-4] 6.1", this, tan(beta), (1 - K_l) / (2 * mu_u)),
        Evaluate(Cb, pvft, muheff, Ff, nh, pv, pnf, ptf, pnf0, ptf0, pnf1, ptf1)
    )

    //discharge loads on silo hoppers
    def dischargeHopperLoad = NumSection(TextToTranslate("_dischargeHopperLoad", SiloSymbols.dictionary), "[1991-4] 6.3.3",
        Evaluate(fiwh, epsilon, Fe, pne, pte, pne0, pte0, pne1, pte1)
    )

    def statecznosc = Section(
        Evaluate(omega, Cteta),
        AssertionRangeLELE("[1993-1-6] D.1.2.1 (4) zastosowania współczynnika Cx=1.0", this, 1.7, omega, 0.5 * (r / t)),
        AssertionRangeLELE("[1993-1-6] D.1.3.1 (3) skorzystania ze wzoru D.21", this, 20, omega / Cteta, 1.63 * (r / t)),
        Evaluate(Cx)
    )

    def naprezeniaKrytycznePoludnikowe = Section(
        Evaluate(sigxRcr, Qx, dwk, alphax, betax, etax, lambdax0, lambdax, lambdapx, chix, sigxRd),
        AssertionLE("[1993-1-6] D.18 możliwości rezygnacji ze sprawdzania wyboczenia południkowego", this, r / t, 0.03 * (E / fy))
    )

    def naprezeniaKrytyczneRownoleznikowe = Section(
        Evaluate(sigtRcr, alphat, betat, etat, lambdat0, lambdat, lambdapt, chiteta, sigtRd),
        AssertionLE("[1993-1-6] D.27 możliwości rezygnacji ze sprawdzania wyboczenia równoleżnikowego", this, r / t, 0.21 * (E / fy)),
        Evaluate(kw)
    )

    def naprezeniaKrytyczneScinajace = Section(
        AssertionRangeLELE("[1993-1-6] D.33 zastosowania współczynnika Cτ=1.0", this, 10, omega, 8.7 * (r / t)),
        Evaluate(Ctau, tauxtRcr, alphatau, betatau, etatau, lambdatau0, lambdatau, lambdaptau, chitau, tauRd),
        AssertionLE("[1993-1-6] D.40 możliwości rezygnacji ze sprawdzania wyboczenia przy ścinaniu", this, r / t, 0.16 * ((E / fy) ^ 0.67))
    )

    def statecznoscKombinacja1 = Section(
        Evaluate(lR, lf, qwzd, NxEd1, MwEd, QtauEd1, sigmaxEd1, sigmatEd1, tauEd1, kx, kt, ktau, ki, zeta1),
        AssertionLE("interakcji naprężeń [1993-1-6] 8.19", this, zeta1, 1)
    )

    def statecznoscKombinacja2 = Section(
        Evaluate(NxEd2, MeEd, QtauEd2, sigmaxEd2, sigmatEd2, tauEd2, zeta2),
        AssertionLE("interakcji naprężeń [1993-1-6] 8.19", this, zeta2, 1)
    )

    def statecznoscLokalnaPodpory = Section(
        Evaluate(tp, tpp, s1, hpp, s2, ns, N1, sigmax1, sigmax1cr, lambdax1, chix1, sigxRd1, sigmax2),
        AssertionLE("nośności lokalnej przy podporze", this, sigmax1, sigxRd1),
        AssertionLE("nośności lokalnej przy podporze", this, sigmax2, sigxRd)
    )

}	

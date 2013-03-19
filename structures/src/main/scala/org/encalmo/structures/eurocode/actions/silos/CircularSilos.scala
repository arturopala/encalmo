package org.encalmo.structures.eurocode.actions.silos

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.calculation.EvalAt
import org.encalmo.document._
import org.encalmo.structures.eurocode.steel.SteelSymbols


/** Circular silos symbols */
object CircularSilosSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "circularSilos"

	//input geometry
	lazy val d1 = symbol(BasicSymbols.d|1) unit SI.mm acc 1                //Średnica zewnętrzna silosu
    lazy val h1 = symbol(BasicSymbols.h|1) unit SI.mm acc 1                //Wysokość komory silosu
    lazy val h2 = symbol(BasicSymbols.h|2) unit SI.mm acc 1                //Wysokość leja silosu
    lazy val de = symbol(BasicSymbols.d|BasicSymbols.e) unit SI.mm acc 1   //Średnica otworu wysypowego leja
    lazy val h3 = symbol(BasicSymbols.h|3) unit SI.mm acc 1                //Prześwit pomiędzy lejem a poziomem terenu
    lazy val h4 = symbol(BasicSymbols.h|4) unit SI.mm acc 1                //Wysokość całkowita silosu
    lazy val r = symbol(BasicSymbols.r) unit "mm" acc 1                    //Promień powłoki silosu
}

/** Thin walled circular silos with steep hopper expressions */
object ThinWalledCircularSlenderSilosWithSteepHopperExpressions extends MapContext {

    import SteelSymbols.{gammas,E,fy,gammaM1}
    import SilosSymbols._
	import CircularSilosSymbols._
	import ParticulateSolidSymbols._
	
	//calculated geometry
	this(dc) = d1-2*t
	this(r) = (d1-t)/2
	this(A) = PI*(dc^2)/4
	this(U) = PI*dc
	this(AU) = A/U
	this(beta) = arctan((dc-de)/(2*h2))
    this(hh) = (dc/2)/tan(beta)
    this(he) = (de/2)/tan(beta)
	this(htp) = (dc*tan(fir))/2
	this(ho) = htp/2
	this(hc) = h1-ho
	this(hb) = hc+hh
	this(hcdc) = hc/dc
	this(tr) = t
	this(h4) = h3+h2+h1
	
	//volumes
	this(Vc) = A*h1
	this(Vh) = 1/3d*PI*h2*(((dc/2)^2)+(dc/2)*(de/2)+((de/2)^2))
	this(V) = Vc+Vh
	this(W) = V*gammau/GRAV
	this(Sc) = U*h1
	this(Sh) = PI*(h2/cos(beta))*(dc+de)
	this(S) = Sc+Sh
	
	//filling symmetrical load
	//  (1) Maximum normal pressure on vertical wall
	this(zo(1)) = 1/(K_u*mu_l)*AU
	this(YJ(1)) = 1-(EUL^(-z/zo(1)))
    this(pho(1)) = (gammau/mu_l)*AU
	this(phf(1)) = fx(pho(1)*YJ(1),z)
	this(phf1) = EvalAt(phf(1), z -> hc/4)
	this(phf2) = EvalAt(phf(1), z -> hc/2)
	this(phf3) = EvalAt(phf(1), z -> 0.75*hc)
	this(phft) = EvalAt(phf(1), z -> hc)
	//  (2) Maximum frictional traction on vertical wall
	this(zo(2)) = 1/(K_u*mu_u)*AU
    this(YJ(2)) = 1-(EUL^(-z/zo(2)))
    this(pho(2)) = (gammau/mu_u)*AU
    this(phf(2)) = fx(pho(2)*YJ(2),z)
	this(pwf) = mu_u * phf(2)
    this(nfzSk) = mu_u*pho(2)*(z-zo(2)*YJ(2))
    this(nfzSkt) = EvalAt(nfzSk, z -> hc)
	//  (3) Maximum vertical load on hopper or silo bottom
    this(Cb)=1.2
	this(zo(3)) = 1/(K_l*mu_l)*AU
    this(YJ(3)) = 1-(EUL^(-z/zo(3)))
    this(pho(3)) = (gammau/mu_l)*AU
	this(pvf) = (pho(3)/K_l)*YJ(3)
	this(pvft) = EvalAt(Cb*pvf, z -> hc)
	
	//filling patch load
	this(ef) = dc/4
	this(Ef) = 2*ef/dc
	this(Cpf) = 0.21*Cop*(1+2*(Ef^2))*(1-(EUL^(-1.5*(hcdc-1))))
	this(ppf) = Cpf*phf(1)
	this(zp) = min(zo(1),hc/2)
	this(ppfzp) = EvalAt(ppf, z -> zp)
	this(s) = (PI*dc)/16
	this(Fpf1) = (PI/2)*s*dc*ppfzp
	
	//discharge symmetrical load
	this(Ch) = 1.15
	this(Cw) = 1.1
	this(phe) = Ch*phf(1)
	this(pwe) = Cw*pwf
	this(phet) = EvalAt(phe, z -> hc)
	this(nezSk) = Cw*mu_u*pho(2)*(z-zo(2)*YJ(2))
    this(nezSkt) = EvalAt(nezSk, z -> hc)
	
	//discharge patch load
    this(Cpe) = 0.42*Cop*(1+2*(Ef^2))*(1-(EUL^(-1.5*(hcdc-1))))
    this(ppe) = Cpe*phe
    this(ppezp) = EvalAt(ppe, z -> zp)
    this(Fpe1) = (PI/2)*s*dc*ppezp
    
    //Loads on silo hoppers and silo bottoms
    
    //filling load on hopper
    this(muheff) = mu_l
    this(Ff) = 1 - (0.2/(1+(tan(beta)/mu_l)))
    this(nh) = 2*(1-0.2)*muheff*cot(beta)
    this(pv) = fx(((gammau*hh)/(nh-1))*((x/hh)-((x/hh)^nh))+pvft*((x/hh)^nh),x)
    this(pnf) = Ff*pv
    this(ptf) = mu_l*Ff*pv
    this(pnf0) = EvalAt(pnf,x -> he)
    this(ptf0) = EvalAt(ptf,x -> he)
    this(pnf1) = EvalAt(pnf,x -> hh)
    this(ptf1) = EvalAt(ptf,x -> hh)
    
    //discharge load on hopper
    this(fiwh) = arctan(mu_l)
    this(epsilon) = fiwh + arcsin(sin(fiwh)/sin(fiim))
    this(Fe) = (1+sin(fiim)*cos(epsilon))/(1-sin(fiim)*cos(beta+epsilon))
    this(pne) = Fe*pv
    this(pte) = mu_l*Fe*pv
    this(pne0) = EvalAt(pne,x -> he)
    this(pte0) = EvalAt(pte,x -> he)
    this(pne1) = EvalAt(pne,x -> hh)
    this(pte1) = EvalAt(pte,x -> hh)
    
    //ciezar wlasny
    this(Grk) = gammas*tr*A/cos(alpha)
    this(Gck) = gammas*t*U
    this(Ghk) = gammas*th*Sh
    this(Gpk) = 10 unit SI.kN
    //this(Gk) = Gck+Ghk+Grk+Gpk
    
    this(Quk) = (2.5 unit SI.kN/SI.m2) * A
    
    //obciazenie sniegiem
    this(sk) = max(0.7,0.007*HM-1.4)
    this(mi1) = 0.8
    this(Ce) = 0.8
    this(Ct) = 1.0
    this(sr) = mi1*Ce*Ct*sk
    this(Qsk) = sr*A/U
    
    //obciazenie wiatrem
    this(vbo) = 22
    this(qbo) = 0.30
    this(coz) = 1.0
    this(crz) = 1.0*((z/(10 unit SI.m))^0.17)
    this(cez) = 2.3*((z/(10 unit SI.m))^0.24)
    this(vmz) = crz*coz*vbo
    this(qpz) = cez*qbo
    this(cscd) = 1.0
    this(cpi) = -0.6
    this(ze) = h4
    this(Re) = d1*EvalAt(vmz,z -> ze)/((15*1E-6) unit SI.m2/SI.s)
    this(Aref) = d1*(h4-h3)
    this(cf) = 1.2+((0.18*log(10*((200 unit SI.m)/d1)))/(1+0.4*log(Re/1E5)))
    this(Fw) = cscd*cf*EvalAt(qpz,z -> ze)*Aref
    this(wemax) = 1.0*EvalAt(qpz,z -> ze)
    this(wi) = cpi*EvalAt(qpz,z -> ze)
    
    this(omega) = h1/sqrt(r*t)
    this(Cx) = 1.0
    this(sigxRcr) = 0.605*Cx*E*(t/r)
    this(Qx) = 25
    this(dwk) = t/Qx*sqrt(r/t)
    this(alphax) = 0.62/(1+1.91*((dwk/t)^1.44))
    this(betax) = 0.6
    this(etax) = 1
    this(lambdax0) = 0.2
    this(lambdapx) = sqrt(alphax/(1-betax))
    this(lambdax) = sqrt(fy/sigxRcr)
    this(chix) = rangeChoiceLE(lambdax,1-betax*(((lambdax-lambdax0)/(lambdapx-lambdax0))^etax),lambdapx,alphax/(lambdax^2))
    this(sigxRd) = fy*chix/gammaM1
    
    this(Cteta) = 1.0
    this(sigtRcr) = 0.92*E*(Cteta/omega)*(t/r)
    this(alphat) = 0.65
    this(betat) = 0.6
    this(etat) = 1
    this(lambdat0) = 0.4
    this(lambdapt) = sqrt(alphat/(1-betat))
    this(lambdat) = sqrt(fy/sigtRcr)
    this(chiteta) = rangeChoiceLE(lambdat,1-betat*(((lambdat-lambdat0)/(lambdapt-lambdat0))^etax),lambdapt,alphat/(lambdat^2))
    this(sigtRd) = fy*chiteta/gammaM1
    this(kw) = 0.46*(1+0.1*sqrt((Cteta/omega)*(t/r)))
    
    this(Ctau) = 1.0
    this(tauxtRcr) = 0.75*E*Ctau*sqrt(1/omega)*((t/r)^1.25)
    this(alphatau) = 0.65
    this(betatau) = 0.6
    this(etatau) = 1
    this(lambdatau0) = 0.4
    this(lambdaptau) = sqrt(alphatau/(1-betatau))
    this(lambdatau) = sqrt(fy/(sqrt(3)*tauxtRcr))
    this(chitau) = rangeChoiceLE(lambdatau,1-betatau*(((lambdatau-lambdatau0)/(lambdaptau-lambdatau0))^etax),lambdaptau,alphatau/(lambdatau^2))
	this(tauRd) = chitau*fy/sqrt(3)

    this(lR) = min(0.1*h1,0.16*r*sqrt(r/t))
    this(lf) = h1-2*lR
	this(qwzd) =  (wemax*kw-wi)*1.5
	val NxEd1 = NxEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
	this(NxEd(1)) = (Gck*(h1-lR)+Grk+Gpk)*1.35 + Quk*1.5
	this(MwEd) = qwzd*d1*(((h1-lR)^2)/2)
	val sigmaxEd1 = sigmaxEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
	this(sigmaxEd1) = MwEd/(PI*t*(r^2)) + NxEd(1)/(2*PI*r*t)
	val sigmatEd1 = sigmatEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej, wg wzoru z [1993-1-6] D.30, "
	this(sigmatEd1) = qwzd*(r/t)
	val QtauEd1 = QtauEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
	this(QtauEd1) = qwzd*d1*(h1-lR)
	val tauEd1 = tauEd(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
	this(tauEd1) = QtauEd1/(PI*r*t)
	val zeta1 = zeta(1) & "na poziomie krawędzi dolnej strefy przypodporowej"
	this(kx) = 1.25+0.75*chix
	this(kt) = 1.25+0.75*chiteta
	this(ktau) = 1.25+0.25*chitau
	this(ki) = (chix*chiteta)^2
	this(zeta1) = ((sigmaxEd1/sigxRd)^kx)-(ki*(sigmaxEd1/sigxRd)*(sigmatEd1/sigtRd))+((sigmatEd1/sigtRd)^kt)+((tauEd1/tauRd)^ktau)
	
	val NxEd2 = NxEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    this(NxEd(2)) = (Gck*(h1-lR)+Grk+Gpk)*1.35 + Quk*1.5 + Qsk*U*1.5 + EvalAt(nezSk, z -> (hc-lR))*U*1.5
    this(MeEd) = Fpe1*(hc-zp-lR)
    val sigmaxEd2 = sigmaxEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    this(sigmaxEd2) = MeEd/(PI*t*(r^2)) + NxEd(2)/(2*PI*r*t)
    val sigmatEd2 = sigmatEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej, wg wzoru z [1993-1-6] D.30, "
    this(sigmatEd2) = EvalAt(phe, z -> zp)*(r/t)
    val QtauEd2 = QtauEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    this(QtauEd2) = Fpe1
    val tauEd2 = tauEd(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    this(tauEd2) = QtauEd2/(PI*r*t)
    val zeta2 = zeta(2) & "na poziomie krawędzi dolnej strefy przypodporowej"
    this(zeta2) = ((sigmaxEd2/sigxRd)^kx)-(ki*(sigmaxEd2/sigxRd)*(sigmatEd2/sigtRd))+((sigmatEd2/sigtRd)^kt)+((tauEd2/tauRd)^ktau)
    
	this(s2) = s1+2*hpp
	this(N1) = ((Gck*h1+Grk+Gpk+Ghk)*1.35 + (Quk + Qsk*U + V*gammau)*1.5)/ns
	this(sigmax1) = N1/(s1*tpp)
	this(sigmax2) = N1/(s2*tp)
	this(sigmax1cr) = 0.605*(1+1.5*((r/hpp)^2)*(tpp/r))*E*(r/t)
	this(lambdax1) = sqrt(fy/sigmax1cr)
    this(chix1) = rangeChoiceLELE(lambdax1,1,lambdax0,1-betax*(((lambdax1-lambdax0)/(lambdapx-lambdax0))^etax),lambdapx,alphax/(lambdax1^2))
	this(sigxRd1) = fy*chix1/gammaM1
	
	// end of context initialization
	lock

}

/** Circular slender silos calculation */
class ThinWalledCircularSlenderSilosWithSteepHopper(
	diameter:Expression,
	heightOfChamber:Expression, 
	heightOfHopper:Expression, 
	thicknessOfChamberWall:Expression, 
	thicknessOfHopperWall:Expression, 
	thicknessOfRing:Expression, 
	heightOfRing:Expression, 
	widthOfColumn:Expression,
	numberOfColumns:Expression, 
	diameterOfOutlet:Expression, 
	particulateSolid:ParticulateSolid,
	wallType:Expression,
	steel:Context
)
extends Calculation {

    import SteelSymbols.{gammas,E,fy,gammaM1}
	import SilosSymbols._
	import CircularSilosSymbols._
	import ParticulateSolidSymbols._
	import ThinWalledCircularSlenderSilosWithSteepHopperExpressions._
	
	acc(0.01)
	
	particulateSolid(ParticulateSolidSymbols.D) = wallType
	
	this add ThinWalledCircularSlenderSilosWithSteepHopperExpressions
	this add particulateSolid
	this add steel
	
	this(d1) = diameter
	this(h1) = heightOfChamber
	this(h2) = heightOfHopper
	this(t) = thicknessOfChamberWall
	this(th) = thicknessOfHopperWall
	this(de) = diameterOfOutlet
	this(h3) = 2.5 unit SI.m
    this(alpha) = 5 unit SI.deg
	this(HM) = 140 unit SI.m
	this(s1) = widthOfColumn
	this(hpp) = heightOfRing
	this(tp) = t
	this(tpp) = thicknessOfRing
	this(ns) = numberOfColumns
	this(gammaM1) = 1.1
    
	//input geometry
	def inputGeometry = NumSection(TextToTranslate("_inputGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(d1,h1,h2,de,t,th,tr,tp,tpp,alpha,h3,h4),this)
	)
	//input assertions
	
	//calculated geometry
	def calculatedGeometry = NumSection(TextToTranslate("_calculatedGeometry",SilosSymbols.dictionary),
		Evaluate(Seq(r,dc,A,U,AU,beta,hh,he,htp,ho,hc,hb,hcdc,Sc,Sh,S),this),
		AssertionL("[1991-4] 1.1.2 (3)",this,hb/dc,10),
		AssertionL("[1991-4] 1.1.2 (3)",this,hb,100 unit SI.m),
		AssertionL("[1991-4] 1.1.2 (3)",this,dc,60 unit SI.m),
		AssertionGE("[1991-4] 5.1 (2)",this,hc/dc,2)
	)
	//volumes
	def volumes = NumSection(TextToTranslate("_volumes",SilosSymbols.dictionary),
		Evaluate(Seq(Vc,Vh,V,W),this)
	)
    def ciezarWlasny = NumSection("Oddziaływania od ciężaru własnego",Evaluate(this,Gck,Ghk,Grk,Gpk))
    def obciazenieUzytkowe = NumSection("Obciążenie użytkowe",Evaluate(this,Quk))
    def obciazenieSniegiem = NumSection("Oddziaływania od obciążenia śniegiem",Evaluate(this,HM,sk,mi1,Ce,Ct,sr,Qsk))
    def obciazenieWiatrem = NumSection("Oddziaływania od obciążenia wiatrem",Evaluate(this,qbo,cez,qpz,cscd,ze,Re,Aref,cf,Fw,wemax,cpi,wi))
	//filling symmetrical load
	def fillingSymmetricalLoad = NumSection(TextToTranslate("_fillingSymmetricalLoad",SilosSymbols.dictionary),"[1991-4] 5.2.1.1",
        NumSection(TextToTranslate("_fillingSymmetricalLoad_1",SilosSymbols.dictionary),
		Evaluate(Seq(zo(1),pho(1),YJ(1),phf(1),phf1,phf2,phf3,phft),this)
		),
		NumSection(TextToTranslate("_fillingSymmetricalLoad_2",SilosSymbols.dictionary),
        Evaluate(Seq(zo(2),pho(2),YJ(2),phf(2),pwf,nfzSk,nfzSkt),this)
        ),
        NumSection(TextToTranslate("_fillingSymmetricalLoad_3",SilosSymbols.dictionary),
        Evaluate(Seq(zo(3),pho(3),YJ(3),pvf),this)
        )
	)
	//filling patch load
	def fillingPatchLoad = NumSection(TextToTranslate("_fillingPatchLoad",SilosSymbols.dictionary),"[1991-4] 5.2.1.2, 5.2.1.4",
        Evaluate(Seq(ef,Ef,Cpf,ppf,zp,ppfzp,s,Fpf1),this)
    )
    //discharge symmetrical load
	def dischargeSymmetricalLoad = NumSection(TextToTranslate("_dischargeSymmetricalLoad",SilosSymbols.dictionary),"[1991-4] 5.2.2.1",
        Evaluate(Seq(Ch,Cw,phe,phet,pwe,nezSk,nezSkt),this)
    )
    //discharge patch load
    def dischargePatchLoad = NumSection(TextToTranslate("_dischargePatchLoad",SilosSymbols.dictionary),"[1991-4] 5.2.2.2, 5.2.2.4",
        Evaluate(Seq(Cpe,ppe,zp,ppezp,Fpe1),this)
    )
    //filling loads on silo hoppers
    def fillingHopperLoad = NumSection(TextToTranslate("_fillingHopperLoad",SilosSymbols.dictionary),"[1991-4] 6.1.2, 6.3.2",
        AssertionL("leja stromego [1991-4] 6.1",this,tan(beta),(1-K_l)/(2*mu_u)),    
        Evaluate(Seq(Cb,pvft,muheff,Ff,nh,pv,pnf,ptf,pnf0,ptf0,pnf1,ptf1),this)
    )
    //discharge loads on silo hoppers
    def dischargeHopperLoad = NumSection(TextToTranslate("_dischargeHopperLoad",SilosSymbols.dictionary),"[1991-4] 6.3.3", 
        Evaluate(Seq(fiwh,epsilon,Fe,pne,pte,pne0,pte0,pne1,pte1),this)
    )
    def statecznosc = Section(
            Evaluate(this,omega,Cteta),
            AssertionRangeLELE("[1993-1-6] D.1.2.1 (4) zastosowania współczynnika Cx=1.0",this,1.7,omega,0.5*(r/t)),
            AssertionRangeLELE("[1993-1-6] D.1.3.1 (3) skorzystania ze wzoru D.21",this,20,omega/Cteta,1.63*(r/t)),
            Evaluate(this,Cx)
    )
    def naprezeniaKrytycznePoludnikowe = Section(
            Evaluate(this,sigxRcr,Qx,dwk,alphax,betax,etax,lambdax0,lambdax,lambdapx,chix,sigxRd),
            AssertionLE("[1993-1-6] D.18 możliwości rezygnacji ze sprawdzania wyboczenia południkowego",this,r/t,0.03*(E/fy)) 
    )
    def naprezeniaKrytyczneRownoleznikowe = Section(
            Evaluate(this,sigtRcr,alphat,betat,etat,lambdat0,lambdat,lambdapt,chiteta,sigtRd),
            AssertionLE("[1993-1-6] D.27 możliwości rezygnacji ze sprawdzania wyboczenia równoleżnikowego",this,r/t,0.21*(E/fy)),
            Evaluate(this,kw)
    )
    def naprezeniaKrytyczneScinajace = Section(
            AssertionRangeLELE("[1993-1-6] D.33 zastosowania współczynnika Cτ=1.0",this,10,omega,8.7*(r/t)),
            Evaluate(this,Ctau,tauxtRcr,alphatau,betatau,etatau,lambdatau0,lambdatau,lambdaptau,chitau,tauRd),
            AssertionLE("[1993-1-6] D.40 możliwości rezygnacji ze sprawdzania wyboczenia przy ścinaniu",this,r/t,0.16*((E/fy)^0.67))
    )
    def statecznoscKombinacja1 = Section(
            Evaluate(this,lR,lf,qwzd,NxEd1,MwEd,QtauEd1,sigmaxEd1,sigmatEd1,tauEd1,kx,kt,ktau,ki,zeta1),
            AssertionLE("interakcji naprężeń [1993-1-6] 8.19",this,zeta1,1)
    )
    def statecznoscKombinacja2 = Section(
            Evaluate(this,NxEd2,MeEd,QtauEd2,sigmaxEd2,sigmatEd2,tauEd2,zeta2),
            AssertionLE("interakcji naprężeń [1993-1-6] 8.19",this,zeta2,1)
    )
    def statecznoscLokalnaPodpory = Section(
            Evaluate(this,tp,tpp,s1,hpp,s2,ns,N1,sigmax1,sigmax1cr,lambdax1,chix1,sigxRd1,sigmax2),
            AssertionLE("nośności lokalnej przy podporze",this,sigmax1,sigxRd1),
            AssertionLE("nośności lokalnej przy podporze",this,sigmax2,sigxRd)
    )
    
}	

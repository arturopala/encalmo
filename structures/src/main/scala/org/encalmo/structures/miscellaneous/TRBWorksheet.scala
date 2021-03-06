package org.encalmo.structures.miscellaneous

import org.encalmo.expression._
import org.encalmo.document._
import org.encalmo.calculation._
import org.encalmo.style.PredefinedStyles
import PredefinedStyles._
import org.encalmo.structures.Worksheet
import org.encalmo.style.PredefinedStyles

class TRBWorksheet extends Worksheet("trb") {

    import BasicSymbols._
        
    // roboty ziemne
    
    val T = BasicSymbols.T is "Planowany czas robót ziemnych (w dniach)" := 13
    val tzm = BasicSymbols.t|"zm" is "Czas trwania zmiany roboczej w ciągu dnia" := 10 unit SI.h
    val Lu = L|u is "Odległość odwozu urobku" := 14 unit SI.km
    val Aw = A|w is "Powierzchnia obrysu zewnętrznego ścian wg projektu" acc 1 := 1061.5 unit SI.m2
    val Ow = O|w is "Obwód obrysu zewnętrznego ścian wg projektu" acc 1 := 133.23 unit SI.m
    val HS = Symbol("HS") is "Wysokość ściany fundamentowej"; HS := 4.20 unit SI.m
    val BS = Symbol("BS") is "Szerokość ściany fundamentowej"; BS := 0.40 unit SI.m
    val HL = Symbol("HL") is "Wysokość ławy fundamentowej"; HL := 0.70 unit SI.m
    val HW = Symbol("HW") is "Głębokość wykopu"; HW := HS+HL
    val BL = Symbol("BL") is "Szerokość ławy fundamentowej"; BL := 1.20 unit SI.m
    val KG = Symbol("KG") is "Kategoria gruntu"; KG := text("III")
    val l = BasicSymbols.l is "Szerokość skarpy wykopu"; l := HW*0.67
    val lo = BasicSymbols.l|o is "Szerokość odsadzki skarpy wykopu"; lo := 0.6 unit SI.m
    val VB = V|B is "Objętość wykopu do wywiezienia" acc 1; VB := Aw*HW
    val VZ = V|Z is "Objętość wykopu do zasypania" acc 1;  VZ := (l/2+lo+(HL-BL)/2)*HW*Ow
    val VC = V|C is "Objętość całkowita wykopu" acc 1; VC := VB+VZ
    
    // koparka
    
    val Qk = Q|"k" is "Objętość łyżki koparki"; Qk := 0.65 unit SI.m3
    val Vzm = V|"zm" is "Objętość zmianowa do wykopania" acc 1; Vzm := VC/T
    val Vh = V|"h" is "Minimalna intensywność godzinowa pracy koparek"; Vh := Vzm/tzm
    
    val tk = t|k is "Czas trwania cyklu roboczego koparki wg tab. 3.14"; tk := 20 unit SI.s
    val nk = n|k is "Ilość cykli koparki na minutę"; nk := Number(60,SI.s)/tk
    val Sn = S|n is "Współczynnik napełnienia łyżki wg tab. 3.13"; Sn := 0.75
    val Ssp = S|"sp" is "Współczynnik spulchnienia gruntu przy odspajaniu wg tab. 3.1"; Ssp := 1.2
    val Ss = S|s is "Współczynnik spoistości gruntu"; Ss := 1/Ssp
    val Sw = S|w is "Współczynnik wykorzystania czasu pracy koparki"; Sw := 0.7
    
    val We = W|e is "Wydajność godzinowa pracy pojedyńczej koparki"; We := (60*nk*Qk*Sn*Ss*Sw).set(SI.m3/SI.h)
    val Nk = N|k is "Liczba potrzebnych koparek" acc 1; Nk := Vh/We
    
    val Rk = R|k is "Promień kopania"; Rk := 9.15 unit SI.m
    val Bk = B|k is "Maksymalna szerokość pojedyńczego rozkopu"; Bk := 1.5*Rk
    val gammao = gamma|"o" is "Gęstość objętościowa gruntu"; gammao := 2000 unit SI.kg/SI.m3
    val Mk = M|k is "Maksymalna masa urobku napełniającego łyżkę"; Mk := Qk*gammao
    val Ml = M|l is "Masa łyżki"; Ml := 567 unit SI.kg
    val Mkl = M|"kl" is "Maksymalna masa łyżki z urobkiem do weryfikacji udźwigu koparki"; Mkl := Mk+Ml
    
    val Tk = T|k is "Rzeczywisty czas wykonania wykopu (w dniach)" acc 1; Tk := VC/(We*Nk*tzm)
    
    // transport
    
    val Vw = V|"w" is "Objętość urobku do wywiezienia" acc 1; Vw := VB*Ssp
    val Ns = N|"s" is "Nośność środka transportu"; Ns := 15000 unit SI.kg
    val Pjt = P|"jt" is "Pojemność użyteczna środka transportu urobku"; Pjt := Ns/(gammao*Ss)
    val tp = t|p is "Czas podstawienia środka transportu"; tp := 2 unit SI.min
    val nc = n|c is "Liczba cykli koparki potrzebnych do załadowania środka transportu" acc 1; nc := Pjt/(Qk*Sn)
    val tz = t|z is "Czas załadunku urobku" unit SI.min acc 0.1; tz := nc*tk/Sw
    val vj = v|j is "Prędkość średnia środka transportu"; vj := 25 unit SI.km/SI.h
    val tj = t|j is "Czas przejazdu do miejsca składowania urobku" unit SI.min acc 0.1; tj := Lu/vj
    val tw = t|w is "Czas wyładunku urobku" unit SI.min; tw := 3 unit SI.min
    val Tj = T|j is "Czas trwania pojedyńczego cyklu tranportowego" unit SI.min acc 0.1; Tj := tp+tz+2*tj+tw
    val Nj = N|j is "Liczba środków transportu potrzebnych dla zapewnienia ciągłej pracy koparki" acc 1; Nj := Tj/tz
    
    // roboty betonowe
    
    val LS = Symbol("LS") is "Sumaryczna długość ścian betonowych wg projektu"; LS := 185 unit SI.m
    val Vb = V|"b" is "Objętość robót betonowych wg projektu"; Vb := HS*BS*LS
    val Vzbr = V|"zbr" is "Objętość zbrojenia"; Vzbr := 0.01*Vb
    val wz = w|z is "Współczynnik zagęszczenia mieszanki betonowej dla K-4 (konsystencja półciekła)"; wz := 1.15
    val Vbet = V|"bet" is "Objętość potrzebnej mieszanki betonowej" acc 1; Vbet := wz*(Vb-Vzbr)
    
    val Ptb = P|"tb" is "Pojemność przyjętego środka transportu betonu"; Ptb := 6.0 unit SI.m3
    val Qpb = Q|"pb" is "Maksymalna wydajność pompy do betonu"; Qpb := 87 unit SI.m3/SI.h
    val Rpb = R|"pb" is "Wysięg poziomy pompy do betonu"; Rpb := 21 unit SI.m
    
    val db = d|"b" is "Grubość betonowanej pojedyńczo warstwy"; db := 50 unit SI.cm
    val twiaz = t|"wiaz" is "Czas do rozpoczęcia wiązania mieszanki betonowej w temp. niższej od 20 C"; twiaz := 1.5 unit SI.h
    val ttb = t|"tb" is "Czas transportu mieszanki betonowej z wytwórni"; ttb := 0.5 unit SI.h
    val Qbet = Q|"bet" is "Maksymalna wydajność betonowania"; Qbet := min(Qpb, 20 unit SI.m3 / SI.h)
    val Lmax = L|"max" is "Maksymalna długość odcinka betonowania do rozpoczęcia procesu wiązania" acc 1; Lmax := (Qbet*(twiaz-ttb))/(db*BS)
    
    val Qwibr = Q|"wibr" is "Nominalna wydajność zagęszczania mieszanki betonowej za pomocą wibratora pogrążalnego Wacker-Neuson z głowicą typu H35"; Qwibr := 9 unit SI.m3/SI.h
    val Dwibr = D|"wibr" is "Średnica oddziaływania głowicy wibratora typu H35"; Dwibr := 60 unit SI.cm
    val Lwibr = L|"wibr" is "Długość pojedyńczego odcinka wibrowania" acc 1; Lwibr := Dwibr/sqrt(2)
    val tz1 = t|"z1" is "Czas zagęszczania mieszanki na pojedyńczym stanowisku"; tz1 := 15 unit SI.s
    val tz2 = t|"z2" is "Czas przełożenia wibratora na nowe stanowisko"; tz2 := 5 unit SI.s
    val Sww = S|"ww" is "Współczynnik wykorzystania czasu pracy wibratora"; Sww := 0.85
    val Qw = Q|w is "Obliczona intensywność godzinowa zagęszczania mieszanki betonowej" unit SI.m3/SI.h; Qw := (Lwibr*BS*db*((3600 unit SI.s)/(tz1+tz2))*Sww).set(SI.m3/SI.h)
    val Nw = N|w is "Liczba potrzebnych wibratorów (na pojedyńczą pracującą pompę do betonu)" acc 1; Nw := ceil(Qbet/min(Qw,Qwibr))
    
    val Fd = F|"d" is "Powierzchnia deskowania" acc 1; Fd := LS*HS*2
    val tdm = t|"dm" is "Czas jednostkowy montażu deskowania"; tdm := 1.6 unit SI.m2/SI.h
    val tdd = t|"dd" is "Czas jednostkowy demontażu deskowania"; tdd := 3.3 unit SI.m2/SI.h
    val nprac = n|"prac" is "Liczba osób pracujących przy deskowaniu"; nprac := 8
    val Tdm = T|"dm" is "Całkowity czas montażu deskowania (w dniach)" acc 1; Tdm := ceil(Fd/(tdm*nprac*tzm))
    val Tdd = T|"dd" is "Całkowity czas demontażu deskowania (w dniach)" acc 1; Tdd := ceil(Fd/(tdd*nprac*tzm))
    
    val Npb = N|"pb" is "Liczba pomp do betonu"; Npb := 1
    val Tbet = T|"bet" is "Czas układania mieszanki betonowej (w dniach)" acc 1; Tbet := ceil(Vbet/(Npb*Qbet*tzm))
    val Tprzer = T|"przer" is "Czas przerwy roboczej do osiągnięcia wymaganej wytrzymałości betonu (w dniach)"; Tprzer := 4
    val Tb = T|"b" is "Rzeczywisty czas róbót betonowych (w dniach)"; Tb := Tdm+Tbet+Tprzer+Tdd

    override val document = Document("",
        PredefinedStyles.stylesConfig,
        Chapter("",
            Section(
                Section("Ćwiczenie projektowe z przedmiotu \"Technologia Robót Budowlanych\". Autor: Artur Opala, album 61315."),
                Section("Prowadzący: dr inż. Michał Podolski, Instytut Budownictwa Politechniki Wrocławskiej. Semestr zimowy 2011/2012.")),
            Section(""),
            
            NumSection("Parametry zadania",Evaluate(HS,BS,HL,BL,KG,T,tzm,Lu)),

            NumSection("Roboty ziemne",
                NumSection("Parametry wykopu",Evaluate(HW,Aw,Ow,l,lo,VB,VZ,VC)),
                NumSection("Parametry koparki CAT 315D R3.1",Evaluate(Qk,Rk)),
                NumSection("Praca koparek",Evaluate(Vzm,Vh,Qk,tk,nk,Sn,Ss,Sw,We,Nk,Bk,Mk,Ml,Mkl)),
                NumSection("Czas robót ziemnych",Evaluate(Tk)),
                NumSection("Transport urobku samochodem MAN TGS 18",Evaluate(Vw,gammao,Ns,Pjt,tp,nc,tz,vj,tj,tw,Tj,Nj))
            ),
            
            NumSection("Roboty betonowe",
                NumSection("Objętość mieszanki betonowej",Evaluate(LS,Vb,Vzbr,wz,Vbet)),
                NumSection("Środek transportu betonu LIEBHERR HTM 604",Evaluate(Ptb)),
                NumSection("Pompa do betonu CIFA KZR/24",Evaluate(Qpb,Rpb)),
                NumSection("Układanie mieszanki betonowej",Evaluate(db,twiaz,ttb,Qbet,Lmax)),
                NumSection("Zagęszczanie mieszanki betonowej",Evaluate(Qwibr,Dwibr,Lwibr,tz1,tz2,Sww,Qw,Nw)),
                NumSection("Deskowanie i rozdeskowanie",Evaluate(Fd,tdm,tdd,nprac,Tdm,Tdd)),
                NumSection("Czas robót betonowych",Evaluate(Tprzer,Npb,Tbet,Tb))
            ),

            Section(style1.marginTop(20), ""),
            Section("Koniec obliczeń."),
            Section(style1.marginTop(20), ""),
            Section(style1.useAlign("right"), "Opracował: Artur Opala")))

}

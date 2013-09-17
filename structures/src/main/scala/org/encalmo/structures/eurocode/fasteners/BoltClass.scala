package org.encalmo.structures.eurocode.fasteners

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document.{Evaluate, Text, NumSection}


trait BoltClassSymbols extends SymbolConfigurator {

  import org.encalmo.expression.BasicSymbols._

  val CLASS = symbol("CLASS").makeNonPrintable
  val fyb = symbol(f|"yb") unit SI.MPa //Charakterystyczna granica plastyczności
  val fub = symbol(f|"ub") unit SI.MPa //Charakterystyczna wytrzymałość na rozciąganie
  val alphav = symbol(alpha|v) //Współczynnik nośności na ścinanie

}

class BoltClass(val name: String, p_fyb: Double, p_fub: Double, p_alphav: Double) extends MapContext("bolt") with BoltClassSymbols {

    CLASS := text(name)
    fyb := p_fyb
    fub := p_fub
    alphav := p_alphav

    def label = text(name)

    def info = NumSection(Text("Bolt Class",dictionary),name,
        Evaluate(fyb,fub)
    )

}

/** BoltClass library */
object BoltClass extends Catalog[BoltClass]("BoltClass") {

    override val map = Map[String,()=>BoltClass](
        "3.6" -> C_3_6 _,
        "4.6" -> C_4_6 _,
        "4.8" -> C_4_8 _,
        "5.6" -> C_5_6 _,
        "5.8" -> C_5_8 _,
        "6.8" -> C_6_8 _,
        "8.8" -> C_8_8 _,
        "10.9" -> C_10_9 _,
        "12.9" -> C_12_9 _
    )

    def C_3_6 = new BoltClass("3.6",180,300,0.6)
    def C_4_6 = new BoltClass("4.6",240,400,0.6)
    def C_4_8 = new BoltClass("4.8",320,400,0.5)
    def C_5_6 = new BoltClass("5.6",300,500,0.6)
    def C_5_8 = new BoltClass("5.8",400,500,0.5)
    def C_6_8 = new BoltClass("6.8",480,600,0.5)
    def C_8_8 = new BoltClass("8.8",640,800,0.6)
    def C_10_9 = new BoltClass("10.9",900,1000,0.5)
    def C_12_9 = new BoltClass("12.9",1080,1200,0.5)

}


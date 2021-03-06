package org.encalmo.structures.eurocode.fasteners

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._

/** HeadedStud symbols */
trait HeadedStudSymbols extends SymbolConfigurator {

	val ID = symbol("ID").makeNonPrintable
	val d = symbol(BasicSymbols.d) unit "mm"
	val hsc = symbol(BasicSymbols.h|"sc") unit "mm"
	val a = symbol(BasicSymbols.a) unit "mm"
	val dh = symbol(BasicSymbols.d|"h") unit "mm"

}

/** HeadedStud context class */
class HeadedStud(name: String, p_d:Int, p_hsc:Int, p_a:Int, p_dh:Int) extends MapContext("headedstud") with HeadedStudSymbols {
	
	ID := text(name)
    d := p_d
    hsc := p_hsc
    a := p_a
    dh := p_dh
	
	def label = this(ID)

	def info = NumSection(Text("HeadedStud",dictionary),name,
		Evaluate(d,hsc,dh,a)
	)

}

/** HeadedStud library */
object HeadedStud extends Catalog[HeadedStud]("HeadedStud") {

	override val map = Map[String,()=>HeadedStud](
		"S3L 3/4 X 4 3/16 MS" -> NELSON_S3L_19_100 _
	)
	
	def NELSON_S3L_19_100 = new HeadedStud("NELSON S3L 3/4 X 4 3/16 MS",19,100,9,31)



}

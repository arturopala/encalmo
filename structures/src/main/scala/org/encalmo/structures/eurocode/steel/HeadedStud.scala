package org.encalmo.structures.eurocode.steel

import org.encalmo.expression._
import org.encalmo.calculation.Context
import org.encalmo.calculation.MapContext
import org.encalmo.calculation.Calculation
import org.encalmo.calculation.SymbolConfigurator
import org.encalmo.document._

/** HeadedStud symbols */
object HeadedStudSymbols extends SymbolConfigurator {

	import BasicSymbols._
	val dictionary, contextId = "headedstud"
	
	val ID = symbol("ID").makeNonPrintable
	val d = symbol(BasicSymbols.d) unit "mm"
	val hsc = symbol(BasicSymbols.h|"sc") unit "mm"
	val a = symbol(BasicSymbols.a) unit "mm"
	val dh = symbol(BasicSymbols.d|"h") unit "mm"

}

/** Common HeadedStud expressions */
object HeadedStudExpressions extends MapContext {

	import HeadedStudSymbols._
	
	lock
}

/** HeadedStud context class */
class HeadedStud(id:String) extends Calculation(Option(id)) {

	import HeadedStudSymbols._
	
	this add HeadedStudExpressions
	
	this(ID) = text(id)
	
	override def label = this(ID)
	
	def info = NumSection(TextToTranslate("HeadedStud",dictionary),id,
		Evaluate(d,hsc,dh,a)
	)
	

}

/** HeadedStud library */
object HeadedStud {
	
	import HeadedStudSymbols._
	
	def apply(s:String):HeadedStud = map.get(s).map(x => x()).getOrElse(throw new IllegalStateException)
	
	val map = Map[String,()=>HeadedStud](
		"S3L 3/4 X 4 3/16 MS" -> NELSON_S3L_19_100 _
	)
	
	lazy val NELSON_S3L_19_100 = new HeadedStud("NELSON S3L 3/4 X 4 3/16 MS"){
		this(d) = 19
		this(hsc) = 100
		this(a) = 9
		this(dh) = 31
		lock
	}



}

package org.encalmo.examples

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
	val d = symbol(BasicSymbols.d) unit "m"
	val hsc = symbol(BasicSymbols.h|"sc") unit "m"
	val a = symbol(BasicSymbols.a) unit "m"
	val dh = symbol(BasicSymbols.d|"h") unit "m"

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
	
	def info = NumSection(TextToTranslate("HeadedStud",dictionary),id,
		Evaluate(Seq(d,hsc,dh,a),this)
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
		this(d) = 19E-3
		this(hsc) = 100E-3
		this(a) = 9E-3
		this(dh) = 31E-3
		lock
	}



}
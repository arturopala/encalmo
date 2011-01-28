package org.encalmo.document

import scala.collection.mutable.Map

/**
 * Document styles config
 * @author artur.opala
 */
case class StylesConfig(
		var expressions:ExprStylesConfig = ExprStylesConfig(),
		var numsections:NumSectionStylesConfig = NumSectionStylesConfig()
	)extends DocumentComponent(null) with NonVisualDocumentComponent {
	
	def update(sym:StylesConfigSymbols.Value,style:Style) = {
		sym match {
			case StylesConfigSymbols.EXPRESSION => expressions = expressions.copy(expression = Option(style))
			case StylesConfigSymbols.EXPR_SYMBOL => expressions = expressions.copy(symbol = Option(style))
			case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => expressions = expressions.copy(symbolDescription = Option(style))
			case StylesConfigSymbols.EXPR_UNRESOLVED => expressions = expressions.copy(unresolved = Option(style))
			case StylesConfigSymbols.EXPR_SUBSTITUTED => expressions = expressions.copy(substituted = Option(style))
			case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => expressions = expressions.copy(partiallyEvaluated = Option(style))
			case StylesConfigSymbols.EXPR_EVALUATED => expressions = expressions.copy(evaluated = Option(style))
			case StylesConfigSymbols.EXPR_NUMBERS => expressions = expressions.copy(numbers = Option(style))
			case StylesConfigSymbols.NUMSECTION => numsections = numsections.copy(numsection = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL0 => numsections = numsections.copy(level0 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL1 => numsections = numsections.copy(level1 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL2 => numsections = numsections.copy(level2 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL3 => numsections = numsections.copy(level3 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL4 => numsections = numsections.copy(level4 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL5 => numsections = numsections.copy(level5 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL6 => numsections = numsections.copy(level6 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL7 => numsections = numsections.copy(level7 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL8 => numsections = numsections.copy(level8 = Option(style))
			case StylesConfigSymbols.NUMSECT_LEVEL9 => numsections = numsections.copy(level9 = Option(style))
			case _ => Unit
		}
	}
	
}

/**
 * StyleConfig symbols enumeration
 * @author artur.opala
 */
object StylesConfigSymbols extends Enumeration {
	type StylesConfigSymbols = Value
	val EXPRESSION,EXPR_SYMBOL,EXPR_UNRESOLVED,EXPR_SUBSTITUTED,EXPR_PARTIALLY_EVALUATED,EXPR_EVALUATED,EXPR_NUMBERS,EXPR_SYMB_DESCRIPTION = Value
	val NUMSECTION,NUMSECT_LEVEL0,NUMSECT_LEVEL1,NUMSECT_LEVEL2,NUMSECT_LEVEL3,NUMSECT_LEVEL4,NUMSECT_LEVEL5,NUMSECT_LEVEL6,NUMSECT_LEVEL7,NUMSECT_LEVEL8,NUMSECT_LEVEL9 = Value
}

/**
 * Expression styles config
 * @author artur.opala
 */
case class ExprStylesConfig(
		expression:Option[Style] = None,
		symbol:Option[Style] = None,
		unresolved:Option[Style] = None,
		substituted:Option[Style] = None,
		partiallyEvaluated:Option[Style] = None,
		evaluated:Option[Style] = None,
		numbers:Option[Style] = None,
		symbolDescription:Option[Style] = None
	){
	
	lazy val part:Map[StylesConfigSymbols.Value,Option[Style]] = Map(
		StylesConfigSymbols.EXPR_SYMBOL -> symbol,
		StylesConfigSymbols.EXPR_UNRESOLVED -> unresolved,
		StylesConfigSymbols.EXPR_SUBSTITUTED -> substituted,
		StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED -> partiallyEvaluated,
		StylesConfigSymbols.EXPR_EVALUATED -> evaluated
	)
	
}

/**
 * NumSection styles config
 * @author artur.opala
 */
case class NumSectionStylesConfig(
		numsection:Option[Style] = None,
		level0:Option[Style] = None,
		level1:Option[Style] = None,
		level2:Option[Style] = None,
		level3:Option[Style] = None,
		level4:Option[Style] = None,
		level5:Option[Style] = None,
		level6:Option[Style] = None,
		level7:Option[Style] = None,
		level8:Option[Style] = None,
		level9:Option[Style] = None
	){
	
	lazy val level:Map[Int,Option[Style]] = Map(
		0 -> level0,
		1 -> level1,
		2 -> level2,
		3 -> level3,
		4 -> level4,
		5 -> level5,
		6 -> level6,
		7 -> level7,
		8 -> level8,
		9 -> level9
	)
	
}
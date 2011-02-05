package org.encalmo.document

import scala.collection.mutable.Map

/**
 * Document styles config
 * @author artur.opala
 */
case class StylesConfig(
		var expressions:ExprStylesConfig = ExprStylesConfig(),
		var numsections:NumSectionStylesConfig = NumSectionStylesConfig(),
		var assertions:AssertionsStylesConfig = AssertionsStylesConfig()
	)extends DocumentComponent(null) with NonVisualComponent {
	
	def update(sym:StylesConfigSymbols.Value,style:Style) = {
		sym match {
			case StylesConfigSymbols.EXPRESSION => expressions = expressions.copy(expression = Option(style))
			case StylesConfigSymbols.EXPR_ROW => expressions = expressions.copy(block = Option(style))
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
			case StylesConfigSymbols.ASSERTION_FALSE => assertions = assertions.copy(iffalse = Option(style))
			case StylesConfigSymbols.ASSERTION_TRUE => assertions = assertions.copy(iftrue = Option(style))
			case StylesConfigSymbols.ASSERTION_UNKNOWN => assertions = assertions.copy(unknown = Option(style))
			case _ => Unit
		}
	}
	
	def apply(sym:StylesConfigSymbols.Value):Option[Style] = {
        sym match {
            case StylesConfigSymbols.EXPRESSION => expressions.expression
            case StylesConfigSymbols.EXPR_ROW => expressions.block
            case StylesConfigSymbols.EXPR_SYMBOL => expressions.symbol
            case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => expressions.symbolDescription
            case StylesConfigSymbols.EXPR_UNRESOLVED => expressions.unresolved
            case StylesConfigSymbols.EXPR_SUBSTITUTED => expressions.substituted
            case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => expressions.partiallyEvaluated
            case StylesConfigSymbols.EXPR_EVALUATED => expressions.evaluated
            case StylesConfigSymbols.EXPR_NUMBERS => expressions.numbers
            case StylesConfigSymbols.NUMSECTION => numsections.numsection
            case StylesConfigSymbols.NUMSECT_LEVEL0 => numsections.level0
            case StylesConfigSymbols.NUMSECT_LEVEL1 => numsections.level1
            case StylesConfigSymbols.NUMSECT_LEVEL2 => numsections.level2
            case StylesConfigSymbols.NUMSECT_LEVEL3 => numsections.level3
            case StylesConfigSymbols.NUMSECT_LEVEL4 => numsections.level4
            case StylesConfigSymbols.NUMSECT_LEVEL5 => numsections.level5
            case StylesConfigSymbols.NUMSECT_LEVEL6 => numsections.level6
            case StylesConfigSymbols.NUMSECT_LEVEL7 => numsections.level7
            case StylesConfigSymbols.NUMSECT_LEVEL8 => numsections.level8
            case StylesConfigSymbols.NUMSECT_LEVEL9 => numsections.level9
            case StylesConfigSymbols.ASSERTION_FALSE => assertions.iffalse
			case StylesConfigSymbols.ASSERTION_TRUE => assertions.iftrue
			case StylesConfigSymbols.ASSERTION_UNKNOWN => assertions.unknown
            case _ => None
        }
    }
	
}

/**
 * StyleConfig symbols enumeration
 * @author artur.opala
 */
object StylesConfigSymbols extends Enumeration {
	type StylesConfigSymbols = Value
	val EXPRESSION,EXPR_ROW,EXPR_SYMBOL,EXPR_UNRESOLVED,EXPR_SUBSTITUTED,EXPR_PARTIALLY_EVALUATED,EXPR_EVALUATED,EXPR_NUMBERS,EXPR_SYMB_DESCRIPTION = Value
	val NUMSECTION,NUMSECT_LEVEL0,NUMSECT_LEVEL1,NUMSECT_LEVEL2,NUMSECT_LEVEL3,NUMSECT_LEVEL4,NUMSECT_LEVEL5,NUMSECT_LEVEL6,NUMSECT_LEVEL7,NUMSECT_LEVEL8,NUMSECT_LEVEL9 = Value
	val ASSERTION_TRUE,ASSERTION_FALSE,ASSERTION_UNKNOWN = Value
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
		symbolDescription:Option[Style] = None,
		block:Option[Style] = None
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

case class AssertionsStylesConfig(
		iftrue:Option[Style] = None,
		iffalse:Option[Style] = None,
		unknown:Option[Style] = None
	)
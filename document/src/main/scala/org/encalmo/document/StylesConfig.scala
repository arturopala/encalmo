package org.encalmo.document

import scala.collection.mutable.Map

/**
 * Document's styles config
 * @author artur.opala
 */
case class StylesConfig(
    style:Style = DefaultStyle
) {
    
    var default:Option[Style] = Option(style)
	var expression:Option[Style] = None
	var symbol:Option[Style] = None
	var unresolved:Option[Style] = None
	var substituted:Option[Style] = None
	var partiallyEvaluated:Option[Style] = None
	var evaluated:Option[Style] = None
	var numbers:Option[Style] = None
	var symbolDescription:Option[Style] = None
	var block:Option[Style] = None
	var numsection:Option[Style] = None
	var level0:Option[Style] = None
	var level1:Option[Style] = None
	var level2:Option[Style] = None
	var level3:Option[Style] = None
	var level4:Option[Style] = None
	var level5:Option[Style] = None
	var level6:Option[Style] = None
	var level7:Option[Style] = None
	var level8:Option[Style] = None
	var level9:Option[Style] = None
	var assert_true:Option[Style] = None
	var assert_false:Option[Style] = None
	var assert_unknown:Option[Style] = None
	
	def update(sym:StylesConfigSymbols.Value,style:Style) = {
		sym match {
		    case StylesConfigSymbols.DEFAULT => default = Option(style)
			case StylesConfigSymbols.EXPRESSION => expression = Option(style)
			case StylesConfigSymbols.EXPR_ROW => block = Option(style)
			case StylesConfigSymbols.EXPR_SYMBOL => symbol = Option(style)
			case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => symbolDescription = Option(style)
			case StylesConfigSymbols.EXPR_UNRESOLVED => unresolved = Option(style)
			case StylesConfigSymbols.EXPR_SUBSTITUTED => substituted = Option(style)
			case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => partiallyEvaluated = Option(style)
			case StylesConfigSymbols.EXPR_EVALUATED => evaluated = Option(style)
			case StylesConfigSymbols.EXPR_NUMBERS => numbers = Option(style)
			case StylesConfigSymbols.NUMSECTION => numsection = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL0 => level0 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL1 => level1 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL2 => level2 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL3 => level3 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL4 => level4 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL5 => level5 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL6 => level6 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL7 => level7 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL8 => level8 = Option(style)
			case StylesConfigSymbols.NUMSECT_LEVEL9 => level9 = Option(style)
			case StylesConfigSymbols.ASSERTION_FALSE => assert_false = Option(style)
			case StylesConfigSymbols.ASSERTION_TRUE => assert_true = Option(style)
			case StylesConfigSymbols.ASSERTION_UNKNOWN => assert_unknown = Option(style)
			case _ => Unit
		}
	}
	
	def apply(sym:StylesConfigSymbols.Value):Option[Style] = {
        sym match {
            case StylesConfigSymbols.DEFAULT => default
            case StylesConfigSymbols.EXPRESSION => expression
            case StylesConfigSymbols.EXPR_ROW => block
            case StylesConfigSymbols.EXPR_SYMBOL => symbol
            case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => symbolDescription
            case StylesConfigSymbols.EXPR_UNRESOLVED => unresolved
            case StylesConfigSymbols.EXPR_SUBSTITUTED => substituted
            case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => partiallyEvaluated
            case StylesConfigSymbols.EXPR_EVALUATED => evaluated
            case StylesConfigSymbols.EXPR_NUMBERS => numbers
            case StylesConfigSymbols.NUMSECTION => numsection
            case StylesConfigSymbols.NUMSECT_LEVEL0 => level0
            case StylesConfigSymbols.NUMSECT_LEVEL1 => level1
            case StylesConfigSymbols.NUMSECT_LEVEL2 => level2
            case StylesConfigSymbols.NUMSECT_LEVEL3 => level3
            case StylesConfigSymbols.NUMSECT_LEVEL4 => level4
            case StylesConfigSymbols.NUMSECT_LEVEL5 => level5
            case StylesConfigSymbols.NUMSECT_LEVEL6 => level6
            case StylesConfigSymbols.NUMSECT_LEVEL7 => level7
            case StylesConfigSymbols.NUMSECT_LEVEL8 => level8
            case StylesConfigSymbols.NUMSECT_LEVEL9 => level9
            case StylesConfigSymbols.ASSERTION_FALSE => assert_false
			case StylesConfigSymbols.ASSERTION_TRUE => assert_true
			case StylesConfigSymbols.ASSERTION_UNKNOWN => assert_unknown
            case _ => None
        }
    }
	
	def all:Seq[Option[Style]] = Seq(
        expression,
        block,
        symbol,
        symbolDescription,
        unresolved,
        substituted,
        partiallyEvaluated,
        evaluated,
        numbers,
        numsection,
        level0,
        level1,
        level2,
        level3,
        level4,
        level5,
        level6,
        level7,
        level8,
        level9,
        assert_false,
        assert_true,
        assert_unknown
    )

	lazy val part:Map[StylesConfigSymbols.Value,Option[Style]] = Map(
		StylesConfigSymbols.EXPR_SYMBOL -> symbol,
		StylesConfigSymbols.EXPR_UNRESOLVED -> unresolved,
		StylesConfigSymbols.EXPR_SUBSTITUTED -> substituted,
		StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED -> partiallyEvaluated,
		StylesConfigSymbols.EXPR_EVALUATED -> evaluated
	)
	
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
	
	def getStyleClassId(style:Style) = {
	    StylesConfigSymbols.values.find(v => apply(v) == style).map(classId(_))
	}
	
	def classId(sym:StylesConfigSymbols.Value):String = {
        sym match {
            case StylesConfigSymbols.DEFAULT => "default"
            case StylesConfigSymbols.EXPRESSION => "exp"
            case StylesConfigSymbols.EXPR_ROW => "exprow"
            case StylesConfigSymbols.EXPR_SYMBOL => "expsym"
            case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => "expsd"
            case StylesConfigSymbols.EXPR_UNRESOLVED =>"expunr"
            case StylesConfigSymbols.EXPR_SUBSTITUTED => "expsub"
            case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => "exppev"
            case StylesConfigSymbols.EXPR_EVALUATED => "expeva"
            case StylesConfigSymbols.EXPR_NUMBERS => "expnum"
            case StylesConfigSymbols.NUMSECTION => "ns"
            case StylesConfigSymbols.NUMSECT_LEVEL0 => "ns0"
            case StylesConfigSymbols.NUMSECT_LEVEL1 => "ns1"
            case StylesConfigSymbols.NUMSECT_LEVEL2 => "ns2"
            case StylesConfigSymbols.NUMSECT_LEVEL3 => "ns3"
            case StylesConfigSymbols.NUMSECT_LEVEL4 => "ns4"
            case StylesConfigSymbols.NUMSECT_LEVEL5 => "ns5"
            case StylesConfigSymbols.NUMSECT_LEVEL6 => "ns6"
            case StylesConfigSymbols.NUMSECT_LEVEL7 => "ns7"
            case StylesConfigSymbols.NUMSECT_LEVEL8 => "ns8"
            case StylesConfigSymbols.NUMSECT_LEVEL9 => "ns9"
            case StylesConfigSymbols.ASSERTION_FALSE => "assertfalse"
			case StylesConfigSymbols.ASSERTION_TRUE => "assertrue"
			case StylesConfigSymbols.ASSERTION_UNKNOWN => "assertunknown"
            case _ => ""
        }
    }
}

/**
 * StyleConfig symbols enumeration
 * @author artur.opala
 */
object StylesConfigSymbols extends Enumeration {
	type StylesConfigSymbols = Value
	val DEFAULT,EXPRESSION,EXPR_ROW,EXPR_SYMBOL,EXPR_UNRESOLVED,EXPR_SUBSTITUTED,EXPR_PARTIALLY_EVALUATED,EXPR_EVALUATED,EXPR_NUMBERS,EXPR_SYMB_DESCRIPTION = Value
	val NUMSECTION,NUMSECT_LEVEL0,NUMSECT_LEVEL1,NUMSECT_LEVEL2,NUMSECT_LEVEL3,NUMSECT_LEVEL4,NUMSECT_LEVEL5,NUMSECT_LEVEL6,NUMSECT_LEVEL7,NUMSECT_LEVEL8,NUMSECT_LEVEL9 = Value
	val ASSERTION_TRUE,ASSERTION_FALSE,ASSERTION_UNKNOWN = Value
}
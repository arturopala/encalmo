package org.encalmo.style

import scala.collection.mutable

/**
 * Document's styles config
 * @author artur.opala
 */
case class StylesConfig(
    defstyle:Style = DefaultStyle
) {
    
    var default:Style = defstyle
	var expression:Style = default
	var symbol:Style = default
	var unresolved:Style = default
	var substituted:Style = default
	var partiallyEvaluated:Style = default
	var evaluated:Style = default
	var numbers:Style = default
	var symbolDescription:Style = default
	var block:Style = default
	var numsection:Style = default
	var level0:Style = default
	var level1:Style = default
	var level2:Style = default
	var level3:Style = default
	var level4:Style = default
	var level5:Style = default
	var level6:Style = default
	var level7:Style = default
	var level8:Style = default
	var level9:Style = default
	var assert_true:Style = default
	var assert_false:Style = default
	var assert_unknown:Style = default
    var requirement_true:Style = default
    var requirement_false:Style = default
    var requirement_unknown:Style = default
	
	def update(sym:StylesConfigSymbols.Value,style:Style) = {
		sym match {
		    case StylesConfigSymbols.DEFAULT => default = style
			case StylesConfigSymbols.EXPRESSION => expression = style
			case StylesConfigSymbols.EXPR_ROW => block = style
			case StylesConfigSymbols.EXPR_SYMBOL => symbol = style
			case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => symbolDescription = style
			case StylesConfigSymbols.EXPR_UNRESOLVED => unresolved = style
			case StylesConfigSymbols.EXPR_SUBSTITUTED => substituted = style
			case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => partiallyEvaluated = style
			case StylesConfigSymbols.EXPR_EVALUATED => evaluated = style
			case StylesConfigSymbols.EXPR_NUMBERS => numbers = style
			case StylesConfigSymbols.NUMSECTION => numsection = style
			case StylesConfigSymbols.NUMSECT_LEVEL0 => level0 = style
			case StylesConfigSymbols.NUMSECT_LEVEL1 => level1 = style
			case StylesConfigSymbols.NUMSECT_LEVEL2 => level2 = style
			case StylesConfigSymbols.NUMSECT_LEVEL3 => level3 = style
			case StylesConfigSymbols.NUMSECT_LEVEL4 => level4 = style
			case StylesConfigSymbols.NUMSECT_LEVEL5 => level5 = style
			case StylesConfigSymbols.NUMSECT_LEVEL6 => level6 = style
			case StylesConfigSymbols.NUMSECT_LEVEL7 => level7 = style
			case StylesConfigSymbols.NUMSECT_LEVEL8 => level8 = style
			case StylesConfigSymbols.NUMSECT_LEVEL9 => level9 = style
			case StylesConfigSymbols.ASSERTION_FALSE => assert_false = style
			case StylesConfigSymbols.ASSERTION_TRUE => assert_true = style
			case StylesConfigSymbols.ASSERTION_UNKNOWN => assert_unknown = style
            case StylesConfigSymbols.REQUIREMENT_FALSE => requirement_false = style
            case StylesConfigSymbols.REQUIREMENT_TRUE => requirement_true = style
            case StylesConfigSymbols.REQUIREMENT_UNKNOWN => requirement_unknown = style
			case _ => Unit
		}
	}
	
	def apply(sym:StylesConfigSymbols.Value):Style = {
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
            case StylesConfigSymbols.REQUIREMENT_FALSE => requirement_false
            case StylesConfigSymbols.REQUIREMENT_TRUE => requirement_true
            case StylesConfigSymbols.REQUIREMENT_UNKNOWN => requirement_unknown
            case _ => default
        }
    }
	
	def all:Seq[Style] = Seq(
	    default,
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
        assert_unknown,
        requirement_false,
        requirement_true,
        requirement_unknown
    )

	lazy val part:mutable.Map[StylesConfigSymbols.Value,Style] = mutable.Map(
		StylesConfigSymbols.EXPR_SYMBOL -> symbol,
		StylesConfigSymbols.EXPR_UNRESOLVED -> unresolved,
		StylesConfigSymbols.EXPR_SUBSTITUTED -> substituted,
		StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED -> partiallyEvaluated,
		StylesConfigSymbols.EXPR_EVALUATED -> evaluated
	)
	
	lazy val level:mutable.Map[Int,Style] = mutable.Map(
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
	
	def matchStyleClassId(style:Style):Option[String] = {
	    val a = StylesConfigSymbols.values.find(apply(_).classId == style.classId)
	    a.map(v => classId(v)) match {
	        case None => if(style!=null) Some(style.classId) else None
	        case some => some
	    }
	}
	
	def classId(sym:StylesConfigSymbols.Value):String = {
        sym match {
            case StylesConfigSymbols.DEFAULT => "default"
            case StylesConfigSymbols.EXPRESSION => "exp"
            case StylesConfigSymbols.EXPR_ROW => "exprow"
            case StylesConfigSymbols.EXPR_SYMBOL => "expsymb"
            case StylesConfigSymbols.EXPR_SYMB_DESCRIPTION => "expdesc"
            case StylesConfigSymbols.EXPR_UNRESOLVED =>"expunres"
            case StylesConfigSymbols.EXPR_SUBSTITUTED => "expsubst"
            case StylesConfigSymbols.EXPR_PARTIALLY_EVALUATED => "expparteval"
            case StylesConfigSymbols.EXPR_EVALUATED => "expeval"
            case StylesConfigSymbols.EXPR_NUMBERS => "expnumb"
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
			case StylesConfigSymbols.ASSERTION_TRUE => "asserttrue"
			case StylesConfigSymbols.ASSERTION_UNKNOWN => "assertunknown"
            case StylesConfigSymbols.REQUIREMENT_FALSE => "reqfalse"
            case StylesConfigSymbols.REQUIREMENT_TRUE => "reqtrue"
            case StylesConfigSymbols.REQUIREMENT_UNKNOWN => "requnknown"
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
	val ASSERTION_TRUE,ASSERTION_FALSE,ASSERTION_UNKNOWN,REQUIREMENT_TRUE,REQUIREMENT_FALSE,REQUIREMENT_UNKNOWN = Value
}
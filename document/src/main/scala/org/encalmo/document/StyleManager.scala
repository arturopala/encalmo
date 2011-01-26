package org.encalmo.document

import scala.collection.mutable.Map

/**
 * Style list used for automatic NumSection styling
 * @author artur.opala
 */
case class StyleManager(map:Map[StyledPlace,Style] = Map())
extends DocumentComponent(null) with NonVisualDocumentComponent {
	
	def define(sp:StyledPlace,s:Style) = {
		map.put(sp, s)
	}
	
	def get(sp:StyledPlace):Option[Style] = {
		map.get(sp)
	}
	
}

/**
 * StyledPlace class
 * @author artur.opala
 */
case class StyledPlace(id:Symbol)

/**
 * Styled places enumeration
 * @author artur.opala
 */
object StyledPlaces {
	
	val STYLED_PLACE_EXPRESSION:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION)
	val STYLED_PLACE_EXPRESSION_SYMBOL:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_SYMBOL)
	val STYLED_PLACE_EXPRESSION_UNRESOLVED:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_UNRESOLVED)
	val STYLED_PLACE_EXPRESSION_RESOLVED:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_RESOLVED)
	val STYLED_PLACE_EXPRESSION_INTERMEDIATE_EVALUATION:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_INTERMEDIATE_EVALUATION)
	val STYLED_PLACE_EXPRESSION_EVALUATED:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_EVALUATED)
	val STYLED_PLACE_EXPRESSION_NUMBERS:StyledPlace = StyledPlace('STYLED_PLACE_EXPRESSION_NUMBERS)
	
	val STYLED_PLACE_NUM_SECTION:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION)
	val STYLED_PLACE_NUM_SECTION_LEVEL_00:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_00)
	val STYLED_PLACE_NUM_SECTION_LEVEL_01:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_01)
	val STYLED_PLACE_NUM_SECTION_LEVEL_02:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_02)
	val STYLED_PLACE_NUM_SECTION_LEVEL_03:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_03)
	val STYLED_PLACE_NUM_SECTION_LEVEL_04:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_04)
	val STYLED_PLACE_NUM_SECTION_LEVEL_05:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_05)
	val STYLED_PLACE_NUM_SECTION_LEVEL_06:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_06)
	val STYLED_PLACE_NUM_SECTION_LEVEL_07:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_07)
	val STYLED_PLACE_NUM_SECTION_LEVEL_08:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_08)
	val STYLED_PLACE_NUM_SECTION_LEVEL_09:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_09)
	val STYLED_PLACE_NUM_SECTION_LEVEL_10:StyledPlace = StyledPlace('STYLED_PLACE_NUM_SECTION_LEVEL_10)
	
}
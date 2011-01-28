package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.document.StylesConfigSymbols._

object Predefined {
    
    val style1 = DefaultStyle.fontSize(11).useSpaceBefore(2).useSpaceAfter(2).useDistanceBetweenStarts(150).useDistanceLabelSeparation(5)
    val BOLD = style1.fontBold
    val ITALIC = style1.fontItalic
    
    val stylesConfig = StylesConfig()
    stylesConfig(EXPRESSION) = style1.marginLeft(15)
    stylesConfig(EXPR_SYMBOL) = style1
    stylesConfig(EXPR_SYMB_DESCRIPTION) = style1.fontItalic.width(500)
    stylesConfig(EXPR_UNRESOLVED) = style1.fontSmaller
    stylesConfig(EXPR_SUBSTITUTED) = style1.fontSmaller
    stylesConfig(EXPR_PARTIALLY_EVALUATED) = style1.fontSmaller
    stylesConfig(EXPR_NUMBERS) = style1.useColor(java.awt.Color.BLUE)
    stylesConfig(EXPR_EVALUATED) = style1.fontBold
    stylesConfig(NUMSECT_LEVEL0) = style1.fontBold.useSpaceBefore(8)
    stylesConfig(NUMSECT_LEVEL1) = style1.useSpaceBefore(6)
    stylesConfig(NUMSECT_LEVEL2) = style1.fontSmaller
    stylesConfig(NUMSECT_LEVEL3) = style1.fontSmaller.fontItalic
    stylesConfig(NUMSECT_LEVEL4) = style1.fontSmaller.fontSmaller.fontItalic

}
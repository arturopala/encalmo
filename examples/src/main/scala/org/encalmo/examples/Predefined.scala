package org.encalmo.examples

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.document.StylesConfigSymbols._

object Predefined {
	
	val layout = Layout().useLeftMargin(40).useRightMargin(10).useBottomMargin(10)
    
    val style1 = DefaultStyle.fontSize(11).useSpaceBefore(2).useSpaceAfter(2).useDistanceBetweenStarts(212).useDistanceLabelSeparation(40)
    val BOLD = style1.fontBold
    val ITALIC = style1.fontItalic
    
    val stylesConfig = StylesConfig()
    stylesConfig(EXPRESSION) = style1
    stylesConfig(EXPR_ROW) = style1.useSpaceBefore(2).useSpaceAfter(2)
    stylesConfig(EXPR_SYMBOL) = style1.width(35).fontBigger.useColor(java.awt.Color.BLUE)
    stylesConfig(EXPR_SYMB_DESCRIPTION) = style1.fontSmaller.fontSmaller.fontItalic.hyphenateOn
    stylesConfig(EXPR_UNRESOLVED) = style1.fontSmaller.fontSmaller
    stylesConfig(EXPR_SUBSTITUTED) = style1.fontSmaller.fontSmaller
    stylesConfig(EXPR_PARTIALLY_EVALUATED) = style1.fontSmaller.fontSmaller
    //stylesConfig(EXPR_NUMBERS) = style1.useColor(java.awt.Color.BLUE)
    stylesConfig(EXPR_EVALUATED) = style1.fontBold.useColor(java.awt.Color.RED)
    stylesConfig(NUMSECT_LEVEL0) = style1.fontBigger.fontBold.useSpaceBefore(16).useLetterSpacing("105%")
    stylesConfig(NUMSECT_LEVEL1) = style1.useSpaceBefore(15)
    stylesConfig(NUMSECT_LEVEL2) = style1.fontSmaller
    stylesConfig(NUMSECT_LEVEL3) = style1.fontSmaller.fontItalic
    stylesConfig(NUMSECT_LEVEL4) = style1.fontSmaller.fontSmaller.fontItalic
    
    val styleDescription:Style = stylesConfig(EXPR_SYMB_DESCRIPTION).get.marginLeft(23)
    val styleWarunek:Style = style1.marginTop(5).marginBottom(5).marginLeft(23).useBackground(new java.awt.Color(255,255,175)).paddings(5)

}
package org.encalmo.structures

import org.encalmo.expression._
import org.encalmo.calculation._
import org.encalmo.document._
import org.encalmo.printer._
import org.encalmo.printer.document._
import org.encalmo.document.StylesConfigSymbols._

object Predefined {
	
	val layout = Layout().useLeftMargin(35).useRightMargin(15).useBottomMargin(10)
    
    val style1 = DefaultStyle.fontFamily("sans-serif").fontSize(12).useSpaceBefore(1).useSpaceAfter(1)
    val style2 = style1.fontFamily("Stix").fontSize(10)
    val BOLD = style1.fontBold
    val ITALIC = style1.fontItalic
    val BLUE = new java.awt.Color(0,0,255)
	val VIOLET = new java.awt.Color(139,0,207)
	val YELLOW = new java.awt.Color(255,255,175)
    
    val stylesConfig = StylesConfig()
    stylesConfig(EXPRESSION) = style2
    stylesConfig(EXPR_ROW) = style2.useSpaceBefore(2).useSpaceAfter(1)
    stylesConfig(EXPR_SYMBOL) = style2.fontItalic.width(35).useColor(BLUE).fontBigger
    stylesConfig(EXPR_SYMB_DESCRIPTION) = style1.fontSmaller.fontSmaller
    stylesConfig(EXPR_UNRESOLVED) = style2
    stylesConfig(EXPR_SUBSTITUTED) = style2
    stylesConfig(EXPR_PARTIALLY_EVALUATED) = style1.fontSmaller.fontSmaller
    //stylesConfig(EXPR_NUMBERS) = style1.useColor(java.awt.Color.BLUE)
    stylesConfig(EXPR_EVALUATED) = style2/*.fontBold.fontSmaller*//*.useColor(java.awt.Color.BLUE)*/.fontBold
    stylesConfig(NUMSECT_LEVEL0) = style1.fontBold.useSpaceBefore(16).useLetterSpacing("150%")
    stylesConfig(NUMSECT_LEVEL1) = style1.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL2) = style1.fontSmaller.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL3) = style1.fontSmaller.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL4) = style1.fontSmaller.fontItalic.useSpaceBefore(8)
    
    val styleWarunek:Style = style1.fontSmaller.fontSmaller.marginTop(4).marginBottom(4).useBackground(YELLOW).paddings(4).marginLeft(1).paddingLeft(24).paddingRight(0)
    stylesConfig(ASSERTION_TRUE) = styleWarunek
    stylesConfig(ASSERTION_FALSE) = styleWarunek.fontBold.useColor(java.awt.Color.RED)
    stylesConfig(ASSERTION_UNKNOWN) = styleWarunek.useBackground(java.awt.Color.ORANGE).useColor(java.awt.Color.RED)
    
    val styleDescription:Style = style1.fontSmaller.fontSmaller.hyphenateOn.marginLeft(23)
    val styleComment = style1.fontSmaller.marginLeft(20)
    
}

package org.encalmo.structures

import org.encalmo.style.StylesConfigSymbols._
import org.encalmo.printer.Layout
import org.encalmo.style.Style
import org.encalmo.style.DefaultStyle
import org.encalmo.style.StylesConfig
import java.awt.Color

object Predefined {
	
	val layout = Layout().useLeftMargin(35).useRightMargin(15).useBottomMargin(10)
    
    val style1 = DefaultStyle.fontFamily("Stix").fontSize(11).useSpaceBefore(1).useSpaceAfter(1)
    val BOLD = style1.fontBold
    val ITALIC = style1.fontItalic
    val BLUE = new java.awt.Color(0,0,207)
	val VIOLET = new java.awt.Color(139,0,207)
	val YELLOW = new java.awt.Color(255,255,175)
    
    val stylesConfig = StylesConfig()
    val rowStyle = style1.useSpaceBefore(2).useSpaceAfter(1).borders(0.2,"solid",Color.BLACK)
    stylesConfig(EXPRESSION) = style1
    stylesConfig(EXPR_ROW) = rowStyle
    stylesConfig(EXPR_SYMBOL) = style1.fontItalic.width(35).useColor(BLUE)
    stylesConfig(EXPR_SYMB_DESCRIPTION) = style1.fontSmaller.fontSmaller
    stylesConfig(EXPR_UNRESOLVED) = style1.fontSmaller
    stylesConfig(EXPR_SUBSTITUTED) = style1.fontSmaller.fontSmaller
    stylesConfig(EXPR_PARTIALLY_EVALUATED) = style1.fontSmaller.fontSmaller
    //stylesConfig(EXPR_NUMBERS) = style1.useColor(java.awt.Color.BLUE)
    stylesConfig(EXPR_EVALUATED) = style1/*.fontBold.fontSmaller*/.useColor(java.awt.Color.BLUE)
    stylesConfig(NUMSECT_LEVEL0) = style1.fontBold.useSpaceBefore(16).useLetterSpacing("150%")
    stylesConfig(NUMSECT_LEVEL1) = style1.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL2) = style1.fontSmaller.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL3) = style1.fontSmaller.useSpaceBefore(12)
    stylesConfig(NUMSECT_LEVEL4) = style1.fontSmaller.fontItalic.useSpaceBefore(8)

    stylesConfig(REQUIREMENT_TRUE) = rowStyle.borders(0.85,"solid",Color.BLACK).setClassId("reqtrue")
    stylesConfig(REQUIREMENT_FALSE) = rowStyle.borders(1.5,"solid",Color.RED).setClassId("reqfalse")
    stylesConfig(REQUIREMENT_UNKNOWN) = rowStyle.borders(1.5,"solid",Color.ORANGE).setClassId("requnknown")
    
    val styleWarunek:Style = style1.fontSmaller.fontSmaller.marginTop(4).marginBottom(4).useBackground(YELLOW).paddings(4).marginLeft(1).paddingLeft(24).paddingRight(0)
    stylesConfig(ASSERTION_TRUE) = styleWarunek
    stylesConfig(ASSERTION_FALSE) = styleWarunek.fontBold.useColor(java.awt.Color.RED)
    stylesConfig(ASSERTION_UNKNOWN) = styleWarunek.useBackground(java.awt.Color.ORANGE).useColor(java.awt.Color.RED)
    
    val styleDescription:Style = style1.fontSmaller.fontSmaller.hyphenateOn.marginLeft(23).setClassId("description")
    val styleComment = style1.fontSmaller.setClassId("comment")
    val styleComment1 = style1.fontSmaller.marginLeft(20).setClassId("comment1")
    val styleTitle = style1.fontBigger.fontBold.paddingTop(20).paddingBottom(20).useAlign("center").setClassId("title")
    
}

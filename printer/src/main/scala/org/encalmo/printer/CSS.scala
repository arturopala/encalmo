package org.encalmo.printer

import org.encalmo.document.Style
import org.encalmo.document.FontStyle

object CSS {
    
    def convertToClassDefinition(style:Style):String = {
        if(style!=null){
            "."+style.classId.get + " {" +
			attrNoZero("font-family",style.font.family) +
			attrNoZero("font-size",style.font.size,"pt") +
			attrNoZero("font-style",resolveFontStyle(style.font)) +
			attrNoZero("font-weight",resolveFontWeight(style.font)) +
			attrNoZero("color",style.hexColor) +
            attrNoZero("background-color",style.hexBackground) +
			attrNoZero("letter-spacing",style.text.letterSpacing) +
			attrNoZero("word-spacing",style.text.wordSpacing) +
			attrNoZero("line-height",style.text.lineHeight,style.text.unit) +
			attrNoZero("text-align",style.text.align) +
			attrNoZero("text-decoration",style.text.decoration) +
			attrNoZero("text-transform",style.text.transform) +
			attrNoZero("padding-left",style.paragraph.padding.left,style.paragraph.unit) +
			attrNoZero("padding-right",style.paragraph.padding.right,style.paragraph.unit) +
			attrNoZero("min-width",style.paragraph.width,style.paragraph.unit) +
			attrNoZero("min-height",style.paragraph.height,style.paragraph.unit) +
			attrNoZero("space-before",style.paragraph.spaceBefore,style.paragraph.unit) +
			attrNoZero("space-after",style.paragraph.spaceAfter,style.paragraph.unit) +
			attrNoZero("padding-top",style.paragraph.padding.top,style.paragraph.unit) +
			attrNoZero("padding-bottom",style.paragraph.padding.bottom,style.paragraph.unit) +
			attrNoZero("margin-left",style.paragraph.margin.left,style.paragraph.unit) +
			attrNoZero("margin-right",style.paragraph.margin.right,style.paragraph.unit) +
			attrNoZero("margin-top",style.paragraph.margin.top,style.paragraph.unit) +
			attrNoZero("margin-bottom",style.paragraph.margin.bottom,style.paragraph.unit) +
			attrNoZero("text-indent",style.text.indent,style.text.unit) + 
			"}"
		} else ""
    }
    
    def attrNoZero(name:String,value:Any):String = {
		if(value!=null 
				&& value!="0" 
				&& value!=0 
				&& value!=" "
				&& value!=""){
			attr(name,value)
		} else ""
	}
    
    def attrNoZero(name:String,value1:Any ,value2:Any):String = {
		if(value1!=null 
				&& value1!="0" 
				&& value1!=0 
				&& value1!=" "
				&& value1!=""){
			attr(name,value1,value2)
		} else ""
	}
    
    def attr(name:String,value:Any*):String = name+":"+value.mkString+";"
    
    private def resolveFontStyle(fs:FontStyle):String = if(fs.italic){"italic"}else{"normal"}
	private def resolveFontWeight(fs:FontStyle):String = if(fs.bold){"bold"}else{"normal"}

}
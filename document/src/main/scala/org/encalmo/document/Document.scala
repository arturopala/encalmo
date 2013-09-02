package org.encalmo.document
import org.encalmo.style.StylesConfig
import org.encalmo.style.DefaultStyle

/**
 * Document class
 * @author artur
 */
class Document(
        val title:String, 
        val stylesConfig:StylesConfig,
        flow:DocumentComponent*) 
extends DocumentComponentSeq(stylesConfig.default, flow:_*) with BlockComponent {
	
	override def toString = "Document("+customStyle+","+title+","+flow.mkString(",")+")"
	
}

/**
 * Document class companion object
 * @author artur.opala
 */
object Document {
	
	def apply(title:String, stylesConfig:StylesConfig, flow:DocumentComponent*) = {
		new Document(title, stylesConfig, flow:_*)
	}
	
	def apply(title:String, flow:DocumentComponent*) = {
		new Document(title, StylesConfig(DefaultStyle), flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new Document("", StylesConfig(DefaultStyle), flow:_*)
	}
	
	def unapply(d:Document) = Some(d.customStyle,d.title,d.flow)
	
}
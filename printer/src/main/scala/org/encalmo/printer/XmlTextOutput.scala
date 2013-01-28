package org.encalmo.printer
import org.encalmo.style.Style

/**
 * XML text output
 * @author artur.opala
 */
class XmlTextOutput(
	locale:java.util.Locale = java.util.Locale.getDefault, 
	namespace:String = "",
	buffer:StringBuilder = new StringBuilder,
	indent:Indent = new Indent(2)
) 
extends TextOutput(locale,buffer) {	
	
	var attrCounter = 0;
	
	override def open = {
		append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
	}
	
	/**
	 * Appends tag name with namespace prefix if any
	 * @param name tag name
	 */
	private def appendTagName(name:String) = {
		if(namespace!=null && !namespace.isEmpty){
			buffer.append(namespace)
			buffer.append(":")
		}
		buffer.append(name)
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def start(name:String):Unit = {
		if(!indent.isFirst){
			indent.append(buffer)
		} else {
			indent.isFirst = false
		}
		indent ++;
		buffer.append("<")
		appendTagName(name)
	}
	
	def startNoIndent(name:String):Unit = {
        if(!indent.isFirst){
            
        } else {
            indent.isFirst = false
        }
        indent ++;
        buffer.append("<")
        appendTagName(name)
    }
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def startb(name:String):Unit = {
		start(name)
		body
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def start(name:String, classIds:Option[String]*):Unit = {
		start(name)
		if(classIds.exists(_.isDefined)){
		    startAttr("class")
		    buffer.append(classIds.filter(_.isDefined).map(_.get).mkString(" "))
			endAttr()
		}
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def start(name:String, classId:String):Unit = {
		start(name)
		attr("class",classId)
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def startb(name:String, classId:Option[String]*):Unit = {
		start(name,classId:_*)
		body
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def startb(name:String, classId:String):Unit = {
		start(name,classId)
		body
	}
	
	/**
	 * Appends opening tag's closing bracket to the buffer
	 */
	def body:Unit = {
		buffer.append(">")
		indent.inBody = true
		attrCounter = 0
	}
	
	/**
	 * Appends element's end tag to the buffer
	 */
	def end:Unit = {
		buffer.append("/>")
		indent --;
		indent.inBody = false
	}
	
	/**
	 * Appends element's end tag to the buffer
	 */
	def end(name:String):Unit = {
		indent --;
		if(!indent.inBody){
			indent.append(buffer)
		}
		buffer.append("</")
		appendTagName(name)
		buffer.append(">")
		indent.inBody = false
	}
	
	def endNoIndent(name:String):Unit = {
        indent --;
        buffer.append("</")
        appendTagName(name)
        buffer.append(">")
        buffer.append("&#8203;")
        indent.inBody = false
    }
	
	/**
	 * Appends short element's tag to the buffer
	 */
	def elem(name:String):Unit = {
		start(name)
		end
	}
	
	/**
	 * Appends attribute's start to the buffer
	 */
	def startAttr(name:String):Unit = {
		buffer.append(" ")
		buffer.append(name)
		buffer.append("=\"")
	}
	
	/**
	 * Appends attribute's end to the buffer
	 */
	def endAttr():Unit = {
		buffer.append("\"")
		attrCounter = attrCounter + 1
		if(attrCounter>4){
			attrCounter = 0
			indent.append(buffer)
		}
	}
	
	/**
	 * Appends attribute to the buffer
	 */
	def attr(name:String,value:Any*):Unit = {
		startAttr(name)
		value.foreach(buffer.append(_))
		endAttr()
	}
	
	/**
	 * Appends attribute to the buffer
	 * if first value is not zero or empty or null
	 */
	def attrNoZero(name:String,value:Any*):Boolean = {
		if(!value.isEmpty 
				&& value.head!=null 
				&& value.head!="0" 
				&& value.head!=0 
				&& value.head!=" "
				&& value.head!=""){
			attr(name,value:_*)
			true
		} else false
	}
	
	/**
	 * Appends attribute to the buffer
	 * if first value is not zero or empty or null
	 * and if first value not equals the template
	 */
	def attrIfChanged(name:String,template:Any,value:Any*):Boolean = {
		if(!value.isEmpty 
				&& value.head!=null 
				&& value.head!="0" 
				&& value.head!=0 
				&& value.head!=" "
				&& value.head!=""
				&& value.head!=template){
			attr(name,value:_*)
			true
		} else false
	}
	
	/**
	 * Appends boolean attribute to the buffer
	 * if value has changed compared to the template
	 */
	def attrIfChanged(name:String,template:Boolean,value:Boolean):Boolean = {
		if(template!=value){
			if(value){
				attr(name,"true")
			}else{
				attr(name,"false")
			}
			true
		} else false
	}
	
	/**
	 * Appends attribute declaring namespace
	 */
	def declareNamespace(uri:String):Unit = {
		buffer.append(" xmlns")
		if(!namespace.isEmpty){
			buffer.append(":")
			buffer.append(namespace)
		}
		buffer.append("=\"")
		buffer.append(uri)
		buffer.append("\"")
	}
	
	/**
	 * Appends attribute declaring namespace
	 */
	def declareNamespace(namespace:String, uri:String):Unit = {
		buffer.append(" xmlns")
		if(!namespace.isEmpty){
			buffer.append(":")
			buffer.append(namespace)
		}
		buffer.append("=\"")
		buffer.append(uri)
		buffer.append("\"")
	}
	
	/**
	 * Appends style attribute to the buffer
	 */
	def style(attrs:(String,Any)*):Unit = {
		startAttr("style")
		attrs.foreach(attr => {
		    buffer.append(attr._1)
		    buffer.append(":")
		    buffer.append(attr._2)
		    buffer.append(";")
	    })
		endAttr()
	}
	
}

/**
 * Indent manager helper class
 * @author artur.opala
 */
class Indent(spaceCount:Int) {
	
	val INDENT = " " * spaceCount
	
	private var size = 0 
	
	def ++ = {size = size + 1}
	def -- = {size = size - 1}
	
	def reset = {size = 0}
	
	def append(buffer:StringBuilder) = {
		buffer.append("\r\n")
		if(size>0){
			for(x <- 1 to size) buffer.append(INDENT)
		}
	}
	
	var inBody:Boolean = false
	var isFirst:Boolean = true
	
}
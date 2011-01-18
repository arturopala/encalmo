package org.encalmo.printer

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
	
	override def open = {
		append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
	}
	
	/**
	 * Appends tag name with namespace prefix if any
	 * @param name tag name
	 */
	private def appendTagName(name:String) = {
		if(!namespace.isEmpty){
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
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def startb(name:String):Unit = {
		start(name)
		body
	}
	
	/**
	 * Appends opening tag's closing bracket to the buffer
	 */
	def body:Unit = {
		buffer.append(">")
		indent.inBody = true
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
	
	/**
	 * Appends short element's tag to the buffer
	 */
	def elem(name:String):Unit = {
		start(name)
		end
	}
	
	/**
	 * Appends attribute to the buffer
	 */
	def attr(name:String,value:Any*):Unit = {
		buffer.append(" ")
		buffer.append(name)
		buffer.append("=\"")
		value.foreach(buffer.append(_))
		buffer.append("\"")
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
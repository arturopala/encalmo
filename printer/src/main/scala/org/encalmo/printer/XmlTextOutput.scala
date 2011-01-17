package org.encalmo.printer

/**
 * XML text output
 * @author artur.opala
 */
class XmlTextOutput(locale:java.util.Locale = java.util.Locale.getDefault) extends TextOutput(locale) {
	
	override def open = {
		this +\ """<?xml version="1.0" encoding="UTF-8"?>"""
	}
	
	/**
	 * Appends element's start tag to the buffer
	 */
	def start(name:String):Unit = {
		buffer.append("<")
		buffer.append(name)
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
		buffer.append(CRLF)
	}
	
	/**
	 * Appends element's end tag to the buffer
	 */
	def end:Unit = {
		buffer.append("/>")
		buffer.append(CRLF)
	}
	
	/**
	 * Appends element's end tag to the buffer
	 */
	def end(name:String):Unit = {
		buffer.append("</")
		buffer.append(name)
		buffer.append(">")
		buffer.append(CRLF)
	}
	
	/**
	 * Appends short element's tag to the buffer
	 */
	def elem(name:String):Unit = {
		buffer.append("<")
		buffer.append(name)
		buffer.append("/>")
		buffer.append(CRLF)
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
	
}
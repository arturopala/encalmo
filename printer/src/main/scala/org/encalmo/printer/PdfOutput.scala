package org.encalmo.printer

import java.io.OutputStream
import java.io.ByteArrayOutputStream

/**
 * PDF output class
 * @author artur.opala
 */
class PdfOutput(
		val layout:Layout = Layout(),
		val locale:java.util.Locale = java.util.Locale.getDefault
) extends Output[Array[Byte]] { 
	
	val stream:ByteArrayOutputStream = new ByteArrayOutputStream
	
	/**
	 * Opens the output (ex: writes initial statement)
	 */
	override def open() = {}
	
	/**
	 * Appends the content to the output and returns output object
	 */
	def +(content:Array[Byte]):Output[Array[Byte]] = {
		stream.write(content)
		this
	}
	
	/**
	 * Closes the output (ex: writes final statement)
	 */
	def close() = {}
	
	/**
	 * Returns the resulting data
	 */
	def getResult:Array[Byte] = {
		stream.flush()
		stream.toByteArray
	}
	
	def toXslFoOutput:XslFoOutput = {
		new XslFoOutput(layout, locale)
	}
	
	def saveToFile(file:java.io.File) = {
		val bytes = getResult
		using[java.io.FileOutputStream](new java.io.FileOutputStream(file)){
			os => os.write(bytes)
		}
	}

}
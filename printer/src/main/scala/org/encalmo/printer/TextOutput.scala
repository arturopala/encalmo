package org.encalmo.printer

/**
 * Text printer output class
 * @param locale - a Locale object representing a specific geographical, political, or cultural region
 * @author artur.opala
 */
class TextOutput(val locale:java.util.Locale = java.util.Locale.getDefault) extends Output[String] {
	
	val CRLF = "\r\n"
	
	val buffer:StringBuilder = new StringBuilder
	val asWriter:java.io.PrintWriter = new java.io.PrintWriter(new TextOutputWriter(buffer))
	
	def getResult:String = buffer.toString
	
	/**
	 * Appends string to the buffer
	 */
	def +(content:String):TextOutput = {
		buffer.append(content)
		this
	}
	
	/**
	 * Appends integer to the buffer
	 */
	def +(content:Int):TextOutput = {
		buffer.append(content)
		this
	}
	
	/**
	 * Appends float to the buffer
	 */
	def +(content:Float):TextOutput = {
		buffer.append(content)
		this
	}
	
	/**
	 * Appends double to the buffer
	 */
	def +(content:Double):TextOutput = {
		buffer.append(content)
		this
	}
	
	/**
	 * Shorthand: Appends string and content to the buffer
	 */
	def +\(content:String):TextOutput = this + content + "\r\n"
	
	/**
	 * Shorthand: Appends newline and content to the buffer
	 */
	def \+(content:String):TextOutput = this + "\r\n" + content
	
	override def open = Unit
	
	override def close = Unit
	
	def printConsole = Console.println(buffer.toString)

}

class TextOutputWriter(buffer:StringBuilder) extends java.io.Writer {
	
	def write(cbuf:Array[Char],offset:Int,len:Int) = {
		buffer.append(cbuf, offset, len);
	}
	
	def close = Unit
	
	def flush = Unit
}
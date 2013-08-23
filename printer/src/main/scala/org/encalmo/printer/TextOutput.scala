package org.encalmo.printer

/**
 * Text printer output class
 * @param locale - a Locale object representing a specific geographical, political, or cultural region
 * @author artur.opala
 */
class TextOutput(val locale:java.util.Locale = java.util.Locale.getDefault, val buffer:StringBuilder = new StringBuilder) extends Output[String] {
	
    val asWriter:java.io.PrintWriter = new java.io.PrintWriter(new TextOutputWriter(buffer))
    
    val DecimalFormatSymbols = java.text.DecimalFormatSymbols.getInstance(locale)
	val CRLF = "\r\n"
	val COMMA = DecimalFormatSymbols.getPatternSeparator
	val SPACE = " "
	
	def getResult:String = buffer.toString()
	
	/**
	 * Appends string to the buffer
	 */
	def +(content:String):TextOutput = {
		buffer.append(content)
		this
	}
	
	/**
	 * Appends optional string to the buffer
	 */
	def append(opt:Option[String]):Unit = {
		opt.map(buffer.append)
	}
	
	/**
     * Appends char to the buffer
     */
	def append(char:Char):Unit = {
	    buffer.append(char)
	}
	
	/**
	 * Appends string to the buffer
	 */
	def append(content:String*):Unit = {
		for(s <- content){
			if(s!=null){
				buffer.append(s)
			}
		}
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
	def +\(content:String):TextOutput = this + content + CRLF
	
	/**
	 * Shorthand: Appends newline and content to the buffer
	 */
	def \+(content:String):TextOutput = this + CRLF + content
	
	override def open = Unit
	
	override def close = Unit
	
	def printConsole() = Console.println(getResult)
	
	def saveToFile(file:java.io.File) = {
	    file.getParentFile.mkdirs
		val text = getResult
		using[java.io.OutputStreamWriter](new java.io.OutputStreamWriter(new java.io.FileOutputStream(file),"utf-8")){
			os => {
				os.write(text)
				os.flush
			}
		}
	}

}

class TextOutputWriter(buffer:StringBuilder) extends java.io.Writer {
	
	override def write(cbuf:Array[Char],offset:Int,len:Int) = {
		buffer.appendAll(cbuf, offset, len)
	}

    override def close() = Unit

    override def flush() = Unit
}
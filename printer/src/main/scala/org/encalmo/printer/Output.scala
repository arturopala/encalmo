package org.encalmo.printer

import scala.language.postfixOps
import scala.language.reflectiveCalls

/**
 * Printer output trait
 * A - data type
 * @author artur.opala
 */
trait Output[A] {
    
    /**
     * This output's locale
     */
    def locale:java.util.Locale
	
	/**
	 * Opens the output (ex: writes initial statement)
	 */
	def open()
	
	/**
	 * Appends the content to the output and returns output object
	 */
	def +(content:A):Output[A]
	
	final def add(content:A):Output[A] = this + content
	
	/**
	 * Closes the output (ex: writes final statement)
	 */
	def close()
	
	/**
	 * Returns the resulting data
	 */
	def getResult:A
	
	def saveToFile(file:java.io.File)
    
    def using[T <: { def close() }]
	    (resource: T)
	    (block: T => Unit) 
	{
	  try {
	    block(resource)
	  } finally {
	    if (resource != null) resource.close()
	  }
	}


}
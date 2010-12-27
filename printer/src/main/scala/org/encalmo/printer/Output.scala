package org.encalmo.printer

/**
 * Printer output trait
 * A - data type
 * @author artur.opala
 */
trait Output[A] {
	
	/**
	 * Opens output (ex: writes initial statement)
	 */
	def open
	
	/**
	 * Appends content to the output and returns output object
	 */
	def +(content:A):Output[A]
	
	final def add(content:A):Output[A] = this + content
	
	/**
	 * Closes output (ex: writes final statement)
	 */
	def close
	
	/**
	 * Returns resulting data
	 */
	def getResult:A

}
package org.encalmo.document

import java.util._
import scala.collection.immutable.StringOps

/**
 * Translator class
 * @author artur.opala
 */
case class Translator(locale:java.util.Locale, dictionary:String = "dictionary") {
	
	lazy val bundle:ResourceBundle = ResourceBundle.getBundle(dictionary,locale)
	
	def translate(text:String) = {
		try{
			bundle.getString(new StringOps(text).map(c => c match {
				case ' ' => '_'
				case _ => c
			}))
		}
		catch {
			case e:Exception => text
		}
	}

}
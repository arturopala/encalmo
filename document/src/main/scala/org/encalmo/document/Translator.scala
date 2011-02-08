package org.encalmo.document

import java.util.Locale
import java.util.ResourceBundle
import java.util.MissingResourceException
import scala.collection.immutable.StringOps
import scala.collection.mutable.Map

/**
 * Translator class
 * @author artur.opala
 */
object Translator {
	
	val cache:Map[(Locale,String),Option[ResourceBundle]] = Map()
	
	private def init(locale:Locale, dictionary:String):Option[ResourceBundle] = {
		try{
			val bundle = Some(ResourceBundle.getBundle("dictionary."+dictionary,locale))
			cache.put((locale,dictionary), bundle)
			bundle
		}
		catch {
			case e:Exception => {
				cache.put((locale,dictionary), None)
				None
			}
		}
	}
	
	private def bundle(locale:Locale, dictionary:String):Option[ResourceBundle] = {
		cache.get((locale,dictionary)).getOrElse(init(locale,dictionary))
	}
	
	def translate(text:String, locale:java.util.Locale, dictionary:String) = {
		bundle(locale, dictionary) match {
			case Some(rb) => {
				get(rb,text).getOrElse({
					val key2 = new StringOps(text).map(x => x match {
						case ' ' => '_'
						case _ => x
					})
					get(rb,key2).getOrElse(text)
				})
			}
			case None => text
		}
	}
	
	private def get(rb:ResourceBundle,key:String):Option[String] = {
		try{
			Some(rb.getString(key))
		}
		catch{
			case mre:MissingResourceException => None
		}
	}

}
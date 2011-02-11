package org.encalmo.common

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
	
	private def normalizedKey(key:String):String = new StringOps(key).map(x => x match {
		case ' ' => '_'
		case ',' => '_'
		case ':' => '_'
		case _ => x
	})
	
	def translate(key:String, locale:java.util.Locale, dictionary:String):Option[String] = {
		bundle(locale, dictionary) match {
			case Some(rb) => {
				get(rb,key) match {
					case s:Some[String] => s
					case None => get(rb,normalizedKey(key))
				}
			}
			case None => None
		}
	}
	
	def hasTranslation(key:String, locale:java.util.Locale, dictionary:String):Boolean = {
		bundle(locale, dictionary) match {
			case Some(rb) => {
				rb.containsKey(key) || rb.containsKey(normalizedKey(key))
			}
			case None => false
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
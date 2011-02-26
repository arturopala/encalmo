package org.encalmo.common

import java.util.Locale
import java.util.Properties
import java.util.Properties
import java.net.URL
import java.io.InputStreamReader
import java.util.MissingResourceException
import scala.collection.immutable.StringOps
import scala.collection.mutable.Map

/**
 * Translator class
 * @author artur.opala
 */
object Translator {
    
    val cache:Map[(Locale,String),Option[Properties]] = Map()
    
    def init(locale:Locale, dictionary:String):Option[Properties] = {
            val file = find(locale,dictionary)
            if(file.isDefined){
                val bundle = read(file.get)
                cache.put((locale,dictionary), bundle)
                bundle
            } else {
                cache.put((locale,dictionary), None)
                None
            }
        
    }
    
    def find(locale:Locale, dictionary:String):Option[URL] = {
        Seq("_"+locale.toString,"_"+locale.getLanguage+"_"+locale.getCountry,"_"+locale.getLanguage,"").map(
                x => getClass.getResource("/dictionary/"+dictionary+x+".properties")).find(url => url!=null)
    }
    
    def read(url:URL):Option[Properties] = {
        try{
            val p = new Properties()
            p.load(new InputStreamReader(url.openStream, "utf-8"))
            Some(p)
        }
        catch {
            case e:Exception => {
                None
            }
        }
    }
    
    def bundle(locale:Locale, dictionary:String):Option[Properties] = {
        cache.get((locale,dictionary)).getOrElse(init(locale,dictionary))
    }
    
    def normalizedKey(key:String):String = new StringOps(key).map(x => x match {
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
    
    def get(rb:Properties,key:String):Option[String] = {
        Option(rb.getProperty(key))
    }

}
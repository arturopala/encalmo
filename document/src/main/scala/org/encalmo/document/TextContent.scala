package org.encalmo.document

import java.util.Locale
import org.encalmo.common.Translator

/**
 * Text component trait
 * @author artur.opala
 */
trait TextContent {
	
	def text: String
    def dictionary: Option[String]

    def translate(locale: Locale): String = {
        dictionary.map(Translator.translate(text,locale,_).getOrElse(text)).getOrElse(text)
    }

}
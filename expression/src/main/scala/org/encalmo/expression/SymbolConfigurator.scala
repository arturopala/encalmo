package org.encalmo.expression

/**
 * Handy symbols's configurator
 * @author artur.opala
 */
trait SymbolConfigurator {

    def id: String
    def dictionary: Option[String]

    def symbol(symbol:Symbol):Symbol = {
        symbol.dictionary(dictionary).id(id)
    }

    def symbol(name:String):Symbol = {
        Symbol(name).dictionary(Option(dictionary).getOrElse(None)).id(id)
    }

    def symbol(symbol:Symbol, dictionary: String):Symbol = {
        symbol.dict(dictionary).id(id)
    }

}
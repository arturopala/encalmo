package org.encalmo.calculation

import org.encalmo.expression._
import scala.collection.Set

/**
 * Calculation is a hierarchical, mutable context for expressions.
 */
class Calculation(val name: String, dictionary: Option[String] = None) extends MapContext(dictionary) with MutableContextSeq {

    def this(name: String, dict: String) = this(name,Option(dict))

    def label:Expression = text(name)

	/** Inserts new Context */
	override def add(context: Context):Unit = {
		if(opened) super.add(context) else throwContextAlreadyLockedException
	}

    /**
     * Should return unresolved expression mapped to that symbol or None
     */
    override def getExpression(s:Symbol):Option[Expression]= {
        getMappedExpression(s) orElse findNestedExpression(s)
    }

    /**
     * Should return true if exists expression mapped to that symbol
     */
    override def hasExpression(s:Symbol):Boolean = {
        hasMappedExpression(s) || hasNestedExpression(s)
    }

    /**
     * Should return sequence of used mappings
     */
    override def listMappings:Seq[(Symbol,Expression)] = {
        listMappedHere ++ listNestedMappings
    }

    override def listMappings(filter: ((Symbol,Expression))=>Boolean):Seq[(Symbol,Expression)] = {
        listMappedHere(filter) ++ listNestedMappings(filter)
    }

    /**
     * Should return sequence of mapped symbols
     */
    override def listSymbols:Set[Symbol] = {
        listMappedSymbols ++ listNestedSymbols
    }

    override def listSymbols(filter: Symbol=>Boolean): Set[Symbol] = {
        listMappedSymbols(filter) ++ listNestedSymbols(filter)
    }

    override def toString = s"Calculation($name)"

}

/** 
 * Calculation factory. 
 */
object Calculation {

	def apply(): Calculation = new Calculation("")
	
	def apply(name: String):Calculation = new Calculation(name)
	
	def apply(vmap:scala.collection.Map[Symbol,Expression]): Calculation = {
	    Calculation() put (vmap.toSeq:_*)
	}
	
	def apply(entries:(Symbol,Expression)*): Calculation = {
		Calculation() put (entries:_*)
	}
	
}
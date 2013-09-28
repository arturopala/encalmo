package org.encalmo.calculation

import org.encalmo.expression._
import scala.collection.Set

/** 
 * Sequence of Contexts
 */
trait ContextSeq extends Context {

    protected def contexts:Seq[Context]
	
    /**
     * Returns expression mapped to that symbol or None
     */
    override def getExpression(s:Symbol):Option[Expression] = findNestedExpression(s/*,contexts.iterator*/)
    
    /**
     * Resolves symbol in nested contexts
     */
    protected final def findNestedExpression(s:Symbol/*, it:Iterator[Context] = contexts.iterator*/):Option[Expression] = {
        contexts.foreach(_.getExpression(s).map(e => return Some(e)))
        None
    }
    
    override def hasExpression(s:Symbol):Boolean = hasNestedExpression(s)

    protected final def hasNestedExpression(s:Symbol):Boolean = contexts.exists(_.hasExpression(s))

    override def listMappings: Seq[(Symbol,Expression)] = listNestedMappings
    override def listMappings(filter: ((Symbol,Expression))=>Boolean):Seq[(Symbol,Expression)] = listNestedMappings(filter)

    protected final def listNestedMappings: Seq[(Symbol,Expression)] = contexts.flatMap(_.listMappings)
    protected final def listNestedMappings(filter: ((Symbol,Expression))=>Boolean): Seq[(Symbol,Expression)] = contexts.flatMap(_.listMappings(filter))

    override def listSymbols: Set[Symbol] = listNestedSymbols
    override def listSymbols(filter: Symbol=>Boolean): Set[Symbol] = listNestedSymbols(filter)

    protected final def listNestedSymbols: Set[Symbol] = contexts.flatMap(_.listSymbols).toSet
    protected final def listNestedSymbols(filter: Symbol=>Boolean): Set[Symbol] = contexts.flatMap(_.listSymbols(filter)).toSet

    override def listNestedResolvers:Seq[Context] = contexts.flatMap(_.listNestedResolvers) ++ contexts
}
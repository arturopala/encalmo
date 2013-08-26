package org.encalmo.calculation

import scala.annotation.tailrec
import org.encalmo.expression._

/** 
 * Sequence of Contexts
 */
trait ContextSeq extends Context {

    protected def contexts:Seq[Context]
	
    /**
     * Returns expression mapped to that symbol or None
     */
    override def getExpression(s:Symbol):Option[Expression] = findNestedExpression(s,contexts.iterator)
    
    /**
     * Resolves symbol in nested contexts
     */
    @tailrec
    protected final def findNestedExpression(s:Symbol, it:Iterator[Context] = contexts.iterator):Option[Expression] = {
        if(!it.hasNext) None
        else {
            val rawExpr = it.next().getExpression(s)
            if(rawExpr.isDefined) {
                rawExpr
            } else {
                findNestedExpression(s,it)
            }
        }
    }
    
    override def hasExpression(s:Symbol):Boolean = hasNestedExpression(s)

    protected final def hasNestedExpression(s:Symbol):Boolean = contexts.exists(_.hasExpression(s))

    override def listMappings: Seq[(Symbol,Expression)] = listNestedMappings

    protected final def listNestedMappings: Seq[(Symbol,Expression)] = contexts.flatMap(_.listMappings)

    override def listSymbols: Seq[Symbol] = listNestedSymbols

    protected final def listNestedSymbols: Seq[Symbol] = contexts.flatMap(_.listSymbols)

    override def listNestedResolvers:Seq[Context] = contexts.flatMap(_.listNestedResolvers) ++ contexts
}
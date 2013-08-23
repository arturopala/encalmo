package org.encalmo.calculation

import scala.collection.{Set}
import scala.annotation.tailrec
import org.encalmo.expression._

/** 
 * Sequence of ExpressionResolvers
 */
trait ExpressionResolverSeq extends ExpressionResolver {
	
	def resolvers:Seq[ExpressionResolver]
	
    /**
     * Returns expression mapped to that symbol or None
     */
    override def getRawExpression(s:Symbol):Option[Expression] = {
        findRawExpression(s,resolvers.iterator)
    }
    
    /**
     * Resolves symbol in nested contexts
     */
    @tailrec
    private def findRawExpression(s:Symbol,it:Iterator[ExpressionResolver]):Option[Expression] = {
        if(!it.hasNext) None
        else {
            val rawExpr = it.next.getRawExpression(s)
            if(rawExpr.isDefined) {
                rawExpr
            } else {
                findRawExpression(s,it) 
            }
        }
    }
    
    /**
     * Returns expression mapped to that symbol or None
     */
    override def getExpression(s:Symbol, cache: ResultsCache):Option[Expression] = {
        findExpression(s,resolvers.iterator, cache)
    }
    
    /**
     * Resolves symbol in nested contexts
     */
    @tailrec
    private def findExpression(s:Symbol, it:Iterator[ExpressionResolver], cache: ResultsCache):Option[Expression] = {
        if(!it.hasNext){
            None
        }else{
            val expr = it.next().getExpression(s, cache)
            if(expr.isDefined){
                expr
            } else {
                findExpression(s, it, cache)
            }
        }
    }
    
    override def hasExpression(s:Symbol):Boolean = {
        resolvers.exists(c => c.hasExpression(s))
    }
    
    override def listMappings: Seq[(Symbol,Expression)] = {
        resolvers.flatMap(r => r.listMappings)
    }
    
    override def listSymbols: Seq[Symbol] = {
        resolvers.flatMap(r => r.listSymbols)
    }
}
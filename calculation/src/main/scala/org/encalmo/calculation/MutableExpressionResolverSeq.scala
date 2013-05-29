package org.encalmo.calculation

import scala.collection.mutable.{Seq,MutableList}
import scala.annotation.tailrec
import org.encalmo.expression._

/** 
 * Mutable seq of ExpressionResolvers
 */
trait MutableExpressionResolverSeq extends ExpressionResolverSeq {
    
    override val resolvers = MutableList[ExpressionResolver]()
    
    /** Add new resolver */
    def add(resolver:ExpressionResolver):Unit = {
        resolvers += resolver
    }

}
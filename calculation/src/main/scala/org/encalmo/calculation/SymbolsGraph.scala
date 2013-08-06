package org.encalmo.calculation

import org.encalmo.graph.{Graph}
import org.encalmo.expression._
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.generic.Growable

object SymbolsGraph {

    class ExpressionTreeVisitor(rootSymbol:Symbol, graph:Growable[(Symbol,Symbol)]) extends TreeVisitor[Expression] {

        override def onEnter(node:Node[Expression]):Unit = {
            node.element match {
                case symbol:Symbol => graph += ((rootSymbol,symbol))
                case _ => Unit
            }
        }

    }

    def build(context:Context):Graph[Symbol] = {
        val graph = Graph.mutable[Symbol]
        for(symbol <- context.listSymbols) {
            context.getRawExpression(symbol).map(
                expr => {
                    val visitor = new ExpressionTreeVisitor(symbol, graph)
                    expr.visit(visitor)
                }
            )
        }
        graph
    }

}

package org.encalmo.calculation

import org.encalmo.graph.{MutableGraph, Graph}
import org.encalmo.expression._
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.generic.Growable

object SymbolGraph {

    class GraphBuildingExpressionTreeVisitor(rootSymbol:Symbol, graph:MutableGraph[Symbol]) extends TreeVisitor[Expression] {

        override def onEnter(node:Node[Expression]):Unit = {
            node.element match {
                case symbol: Symbol => graph link (symbol,rootSymbol,false)
                case DynamicExpression(symbols, _) => for(symbol <- symbols) {graph link (symbol,rootSymbol,false)}
                case PinnedExpression(context,symbol) => graph link (symbol,rootSymbol,false)
                case _ => Unit
            }
        }

    }

    def build(context:Context):Graph[Symbol] = {
        val graph = Graph[Symbol]()
        val symbols = context.listSymbols
        for(symbol <- symbols) {
            graph.add(symbol)
            context.getExpression(symbol).map(
                expr => {
                    val visitor = new GraphBuildingExpressionTreeVisitor(symbol, graph)
                    expr.visit(visitor)
                }
            )
        }
        graph
    }

}

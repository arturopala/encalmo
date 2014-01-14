package org.encalmo.calculation

import org.encalmo.graph.{MutableGraph, Graph}
import org.encalmo.expression._
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.generic.Growable

object SymbolGraph {

    class ExpressionTreeVisitor(rootSymbol:Symbol, graph:MutableGraph[Symbol]) extends TreeVisitor[Expression] {

        override def onEnter(node:Node[Expression]):Unit = {
            node.element match {
                case symbol: Symbol => graph link (symbol,rootSymbol)
                case DynamicExpression(symbols, _) => for(symbol <- symbols) {graph link (symbol,rootSymbol)}
                case PinnedExpression(context,symbol) => graph link (symbol,rootSymbol)
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
                    val visitor = new ExpressionTreeVisitor(symbol, graph)
                    expr.visit(visitor)
                }
            )
        }
        graph
    }

}

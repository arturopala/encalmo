package org.encalmo.calculation

import org.encalmo.graph.Graph
import org.encalmo.expression._
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.generic.Growable

object SymbolGraph {

    class ExpressionTreeVisitor(rootSymbol:Symbol, graph:Growable[(Symbol,Symbol)]) extends TreeVisitor[Expression] {

        override def onEnter(node:Node[Expression]):Unit = {
            node.element match {
                case symbol: Symbol => graph += ((symbol,rootSymbol))
                case DynamicExpression(symbols, _) => for(symbol <- symbols) {graph += ((symbol,rootSymbol))}
                case _ => Unit
            }
        }

    }

    def build(context:ExpressionResolver):Graph[Symbol] = {
        val graph = Graph[Symbol]()
        for(symbol <- context.listSymbols) {
            graph.add(symbol)
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

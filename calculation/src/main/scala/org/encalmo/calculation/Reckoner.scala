package org.encalmo.calculation

import org.encalmo.expression._
import org.encalmo.graph.Graph
import org.encalmo.expression.Selection
import org.encalmo.expression.min
import org.encalmo.expression.max
import org.encalmo.expression.Value
import scala.annotation.tailrec

/**
 * Reckons calculations and creates {@link Results}
 */
object Reckoner {

    def reckon(implicit context: Context): Results = {
        val graph: Graph[Symbol] = SymbolGraph.build(context)
        reckonGraph(graph,context)
    }

    def reckonFor(targetSymbols: Traversable[Symbol])(implicit context: Context): Results = {
        val graph: Graph[Symbol] = SymbolGraph.build(context)
        val filteredGraph: Graph[Symbol] = graph.filterOutPathsNotLeadingTo(targetSymbols)
        Console.println(s"graph nodes: ${graph.nodesCount}, target symbols: ${targetSymbols.size}, nodes reduced to: ${filteredGraph.nodesCount}")
        reckonGraph(filteredGraph,context)
    }


    private def reckonGraph(graph: Graph[Symbol],context: Context): Results = {
        val symbols: List[Symbol] = Graph.sortTopologically(graph)
        Console.println(s"Found ${symbols.size} symbols to reckon in $context:")
        val formulaSet = new FormulaSet()
        val cache = new ResultsCache()
        val results = new Results(graph, formulaSet, cache)
        symbols.foldLeft[FormulaSet](results.formulaSet)((set, symbol) => set put reckonExpression(symbol, context, results))
        Console.println()
        results
    }

    def reckonExpression(expression: Expression, context: Context, results: Results): Formula = {
        results.formulaSet.get(expression) getOrElse {
            expression match {
                case pinned: PinnedExpression => results.formulaSet.get(pinned.symbol) getOrElse reckonExpression(pinned.symbol,pinned.context,results)
                case other => {
                    import org.encalmo.calculation.FormulaPosition._
                    import Relation._
                    val unit: UnitOfValue = expression.unit
                    val accuracy: Option[Double] = expression match {
                        case symbol: Symbol => symbol.accuracy
                        case _ => None
                    }
                    var list: List[Expression] = Nil
                    try {
                        list = prepareUnresolved(list, expression, context, results.cache)
                        list = prepareSubstituted(list, list.head, unit, accuracy, context, results.cache)
                        list = preparePartiallyEvaluated(list, list.head, unit, accuracy, context, results.cache)
                        list = prepareEvaluated(list, list.head, unit, accuracy, context, results.cache)
                        expression match {
                            case symbol: Symbol => {
                                results.cache.put(symbol, list.head)
                            }
                            case _ =>
                        }
                        val items: List[Expression] = list.foldRight(List(expression))((e, l) => if (e != l.head) e :: l else l)
                        val formula = items match {
                            case Nil => throw new IllegalStateException()
                            case List(e1) => Formula(Seq(FormulaPart(e1, LEFT, 0)))
                            case List(e2, e1) => Formula(Seq(FormulaPart(e1, LEFT, 0), FormulaPart(e2, RIGHT, 1)))
                            case List(e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT,0), FormulaPart(e2, EXPR_UNRESOLVED,1), FormulaPart(e3, RIGHT,2)))
                            case List(e4, e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT,0), FormulaPart(e2, EXPR_UNRESOLVED,1), FormulaPart(e3, EXPR_SUBSTITUTED,2), FormulaPart(e4, RIGHT,3)))
                            case List(e5, e4, e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT,0), FormulaPart(e2, EXPR_UNRESOLVED,1), FormulaPart(e3, EXPR_SUBSTITUTED,2), FormulaPart(e4, EXPR_PARTIALLY_EVALUATED,3), FormulaPart(e5, RIGHT,4)))
                            case _ => {
                                val ritems = items.reverse
                                Formula(Seq(FormulaPart(ritems.head, LEFT,0), FormulaPart(ritems.tail.head, EXPR_UNRESOLVED,1), FormulaPart(ritems.tail.tail.head, EXPR_SUBSTITUTED, 2)) ++ (for (i <- 3 until (items.size - 1)) yield FormulaPart(ritems(i), EXPR_PARTIALLY_EVALUATED, i)) :+ FormulaPart(items.head, RIGHT,items.size-1))
                            }
                        }
                        Console.println(formula.face)
                        formula
                    }
                    catch {
                        case exc: Exception => {
                            Console.err.println(s"\r\nCould not reckon expression: ${expression.face} -> ${list.map(_.face).mkString(" -> ")}\r\nCause: " + exc.getMessage)
                            throw exc
                        }
                    }
                }
            }
        }
    }

    @tailrec
    private def prepareUnresolved(list: List[Expression], expression: Expression, context: Context, cache: ResultsCache): List[Expression] = {
        val unresolved = expression match {
            case symbol: Symbol => {
                context.getExpression(symbol).map(processUnresolved(_, context, cache)).getOrElse(symbol)
            }
            case other => other
        }
        unresolved match {
            case symbol2: Symbol if symbol2 ne expression => {
                prepareUnresolved(symbol2 :: list, symbol2, context, cache)
            }
            case other => other :: list
        }
    }

    private def processUnresolved(e: Expression, context: Context, cache: ResultsCache): Expression = {
        e match {
            case ev: EvalAt => {
                EvalAt(ev.expr, ev.er.evaluateWithAndReturnCopy(context, cache))
            }
            case de: DynamicExpression => de.f(cache)
            case _ => e
        }
    }

    private def prepareSubstituted(list: List[Expression], expression: Expression, unit: UnitOfValue, accuracy: Option[Double], context: Context, cache: ResultsCache): List[Expression] = {
        context.substitute(expression)(cache) :: list
    }

    private def preparePartiallyEvaluated(list: List[Expression], expression: Expression, unit: UnitOfValue, accuracy: Option[Double], context: Context, cache: ResultsCache): List[Expression] = {
        if (expression.countTreeLeafs > 3 || expression.isInstanceOf[Selection] || expression.isInstanceOf[Assert]) {
            adjustUnits(context.partiallyEvaluate(expression)(cache), unit, None) :: list
        } else {
            adjustUnits(expression, unit, None) :: list
        }
    }

    private def prepareEvaluated(list: List[Expression], expression: Expression, unit: UnitOfValue, accuracy: Option[Double], context: Context, cache: ResultsCache): List[Expression] = {
        adjustUnits(context.evaluate(expression)(cache), unit, accuracy) :: list
    }

    private def unitAdjustor(unit: UnitOfValue, accuracy: Option[Double]): Transformation = {
        case v: Value => v.convertIfPossibleTo(unit, None)
        case other => other
    }

    private def adjustUnits(expression: Expression, unit: UnitOfValue, accuracy: Option[Double]): Expression = expression match {
        case v: Value => v.convertTo(unit, accuracy)
        case a: Assert if a.unit ne EmptyUnitOfValue => a.map(unitAdjustor(a.unit, accuracy))
        case other => other.map(unitAdjustor(unit, accuracy))
    }

}

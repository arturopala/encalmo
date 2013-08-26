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

    def reckon(results: Results = new Results())(implicit context: Context): Results = {
        val reckonContextFx = reckonContext(results)(_:Context)
        val contexts = /*context.listNestedResolvers :+ context filter (_.isInstanceOf[Calculation])*/Seq(context)
        if(contexts.size>1) Console.println(s"Found ${contexts.size-1} nested contexts to reckon in $context")
        for(context <- contexts){
            val formulaSet = reckonContextFx(context)
            results put (context, formulaSet)
        }
        results
    }

    private[calculation] def reckonContext(results: Results = new Results())(implicit context: Context): FormulaSet = {
        results.get(context) getOrElse {
            val graph = SymbolGraph.build(context)
            val symbols = Graph.sortTopologically(graph)
            Console.println(s"Found ${symbols.size} symbols to reckon in $context: ")
            val formulaSet = symbols.foldLeft[FormulaSet](FormulaSet(context.id))((set, symbol) => set put reckonExpression(symbol,results,set.cache)(context))
            Console.println()
            formulaSet
        }
    }

    private[calculation] def reckonExpression(expression: Expression, results: Results = new Results(), cache: ResultsCache = new ResultsCache())(implicit context: Context): Formula = {
        results.get(context, expression) getOrElse {
            expression match {
                case pinned: PinnedExpression => results.formulaSetFor(pinned.context).get(pinned.symbol) getOrElse reckonExpression(pinned.symbol,results,cache)(pinned.context)
                case other => {
                    import org.encalmo.calculation.FormulaPosition._
                    import org.encalmo.calculation.FormulaPartRelation._
                    val unit: UnitOfValue = expression.unit
                    val accuracy: Option[Double] = expression match {
                        case symbol: Symbol => symbol.accuracy
                        case _ => None
                    }
                    var list: List[Expression] = Nil
                    try {
                        list = prepareUnresolved(list, expression, context, cache)
                        list = prepareSubstituted(list, list.head, unit, accuracy, context, cache)
                        list = preparePartiallyEvaluated(list, list.head, unit, accuracy, context, cache)
                        list = prepareEvaluated(list, list.head, unit, accuracy, context, cache)
                        expression match {
                            case symbol: Symbol => {
                                cache.put(symbol, list.head)
                                Console.print(symbol.simpleFace + ",")
                            }
                            case _ =>
                        }
                        val items = list.foldRight(List(expression))((e, l) => if (e != l.head) e :: l else l)
                        items match {
                            case Nil => throw new IllegalStateException()
                            case List(e1) => Formula(Seq(FormulaPart(e1, LEFT, NONE)))
                            case List(e2, e1) => Formula(Seq(FormulaPart(e1, LEFT, NONE), FormulaPart(e2, RIGHT)))
                            case List(e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT, NONE), FormulaPart(e2, EXPR_UNRESOLVED, EQUAL), FormulaPart(e3, RIGHT)))
                            case List(e4, e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT, NONE), FormulaPart(e2, EXPR_UNRESOLVED, EQUAL), FormulaPart(e3, EXPR_SUBSTITUTED, EQUAL), FormulaPart(e4, RIGHT)))
                            case List(e5, e4, e3, e2, e1) => Formula(Seq(FormulaPart(e1, LEFT, NONE), FormulaPart(e2, EXPR_UNRESOLVED, EQUAL), FormulaPart(e3, EXPR_SUBSTITUTED, EQUAL), FormulaPart(e4, EXPR_PARTIALLY_EVALUATED, EQUAL), FormulaPart(e5, RIGHT)))
                            case _ => {
                                val ritems = items.reverse
                                Formula(Seq(FormulaPart(ritems.head, LEFT, NONE), FormulaPart(ritems.tail.head, EXPR_UNRESOLVED, EQUAL), FormulaPart(ritems.tail.tail.head, EXPR_SUBSTITUTED, EQUAL)) ++ (for (i <- 3 until (items.size - 1)) yield FormulaPart(ritems(i), EXPR_PARTIALLY_EVALUATED)) :+ FormulaPart(items.head, RIGHT))
                            }
                        }
                    }
                    catch {
                        case exc: Exception => {
                            Console.err.println(s"\r\nCould not reckon expression: $expression\r\n$list\r\nCause: " + exc.getMessage)
                            throw exc
                        }
                    }
                }
            }
        }
    }

    @tailrec
    private def prepareUnresolved(list: List[Expression], expression: Expression, context: Context, cache: ResultsCache): List[Expression] = {
        (expression match {
            case symbol: Symbol => {
                context.getExpression(symbol).map(processUnresolved(_, context, cache)).getOrElse(symbol)
            }
            case other => other
        }) match {
            case symbol: Symbol if symbol != expression => {
                prepareUnresolved(symbol :: list, symbol, context, cache)
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
        if (expression.countTreeLeafs > 3 || expression.isInstanceOf[Selection]) {
            adjustUnits(context.partiallyEvaluate(expression)(cache), unit, None) :: list
        } else {
            expression :: list
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
        case other => other.map(unitAdjustor(unit, accuracy))
    }

}

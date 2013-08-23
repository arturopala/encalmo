package org.encalmo.calculation

import org.encalmo.expression._
import org.encalmo.graph.Graph
import org.encalmo.expression.Selection
import org.encalmo.expression.min
import org.encalmo.expression.max
import org.encalmo.expression.Value
import scala.annotation.tailrec

object FormulaReckoner {

    def reckonAll(implicit context: ExpressionResolver, cache: ResultsCache = new ResultsCache()): FormulaSet = {
        val id = context match {
            case c: Context => c.id;
            case _ => None
        }
        val graph = SymbolGraph.build(context)
        val symbols = Graph.sortTopologically(graph)
        Console.println(s"\r\nFound ${symbols.size} symbols to reckon in $context: ")
        val formulaSet = symbols.foldLeft[FormulaSet](FormulaSet(id))((set, symbol) => set += reckon(symbol, cache)(context))
        Console.println()
        formulaSet
    }

    @tailrec
    def reckon(expression: Expression, cache: ResultsCache = new ResultsCache())(implicit context: ExpressionResolver): Formula = {
        expression match {
            case pinned: PinnedExpression => reckon(pinned.symbol, cache)(pinned.context)
            case _ => {
                import org.encalmo.calculation.FormulaPosition._
                import org.encalmo.calculation.FormulaPartRelation._
                val unit: UnitOfValue = expression.unit
                val accuracy: Option[Double] = expression match {
                    case symbol: Symbol => symbol.accuracy;
                    case _ => None
                }
                var list: List[Expression] = Nil
                try {
                    list = prepareUnresolved(list, expression, context, cache)
                    list = adjustUnits(context.substitute(list.head)(cache), unit, accuracy) :: list
                    list = processSubstituted(list.head, unit, accuracy, context, cache) :: list
                    list = adjustUnits(context.evaluate(list.head)(cache), unit, accuracy) :: list
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
                        Console.println(s"Could not reckon expression: $expression\r\n$list\r\nCause: " + exc.getMessage)
                        throw exc
                    }
                }
            }
        }
    }

    @tailrec
    def prepareUnresolved(list: List[Expression], expression: Expression, context: ExpressionResolver, cache: ResultsCache): List[Expression] = {
        (expression match {
            case symbol: Symbol => {
                context.getRawExpression(symbol).map(processUnresolved(_, context, cache)).getOrElse(symbol)
            }
            case other => other
        }) match {
            case symbol: Symbol if symbol != expression => {
                prepareUnresolved(symbol :: list, symbol, context, cache)
            }
            case other => other :: list
        }
    }

    def processUnresolved(e: Expression, context: ExpressionResolver, cache: ResultsCache): Expression = {
        e match {
            case ev: EvalAt => {
                EvalAt(ev.expr, ev.er.evaluateWithAndReturnCopy(context, cache))
            }
            case de: DynamicExpression => de.f(cache)
            case _ => e
        }
    }

    def processSubstituted(expression: Expression, unit: UnitOfValue, accuracy: Option[Double], context: ExpressionResolver, cache: ResultsCache): Expression = {
        val depth = expression.countTreeLeafs
        if (depth > 3 || expression.isInstanceOf[Selection]) {
            adjustUnits((
                expression match {
                    case tr: Transparent => tr.children.head
                    case x => x
                }) match {
                // partially evaluated expression
                case null => expression
                case o: Operation2 => {
                    o.copy(context.evaluate(o.l)(cache), context.evaluate(o.r)(cache))
                }
                case o: OperationN => {
                    o.copy(o.args.map(context.evaluate(_)(cache)): _*)
                }
                case sel: Selection => {
                    context.substitute(sel.select)(cache)
                }
                case _ => context.evaluate(expression)(cache)
            }, unit, accuracy
            )
        } else expression
    }

    def adjustUnits(expression: Expression, unit: UnitOfValue, accuracy: Option[Double]): Expression = unit match {
        case EmptyUnitOfValue => expression
        case _ => expression match {
            case v: Value => v.convertTo(unit, accuracy)
            case s: Sum => if (s.args.exists(_.isInstanceOf[Value])) Sum(s.args.map(b => b match {
                case v: Value => v.convertTo(unit, accuracy)
                case _ => b
            }): _*)
            else s
            case s: min => if (s.args.exists(_.isInstanceOf[Value])) min(s.args.map(b => b match {
                case v: Value => v.convertTo(unit, accuracy)
                case _ => b
            }): _*)
            else s
            case s: max => if (s.args.exists(_.isInstanceOf[Value])) max(s.args.map(b => b match {
                case v: Value => v.convertTo(unit, accuracy)
                case _ => b
            }): _*)
            else s
            case _ => expression
        }
    }

}

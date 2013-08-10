package org.encalmo.calculation

import org.encalmo.expression._
import org.encalmo.graph.Graph

object FormulaReckoner {

  def reckonAll(implicit context: Context): FormulaSet = {
    val graph = SymbolGraph.build(context)
    val symbols = Graph.sortTopologically(graph)
    symbols.foldLeft[FormulaSet](FormulaSet())((set, symbol) => set += reckon(symbol))
  }

  def reckon(e: Expression)(implicit context: Context): Formula = {

    import org.encalmo.calculation.FormulaPosition._
    import org.encalmo.calculation.FormulaPartRelation._

    var parts = Vector[FormulaPart]()
    var ue = e // unresolved expression
    val unit: UnitOfValue = e.unit
    val evaluated = adjustUnitsInSum(context.evaluate(e), unit) // evaluated expression
    e match {
      case boundexpr: BoundExpression => {
        parts = parts :+ FormulaPart(boundexpr.symbol, LEFT, NONE)
        ue = processUnresolved(boundexpr.symbol, boundexpr.er.getRawExpression(boundexpr.symbol) match {
          case Some(x) => x
          case None => e
        }, context)
        if (ue != e && ue != evaluated) {
          parts = parts :+ FormulaPart(ue, EXPR_UNRESOLVED)
        }
      }
      case symbol: Symbol => {
        ue = processUnresolved(symbol, context.getRawExpression(symbol) match {
          case Some(x) => x
          case None => e
        }, context)
        parts = parts :+ FormulaPart(e, LEFT, NONE)
        if (ue != e && ue != evaluated) {
          parts = parts :+ FormulaPart(ue, EXPR_UNRESOLVED)
        }
      }
      case _ => {
        if (ue != evaluated) {
          parts = parts :+ FormulaPart(ue, EXPR_UNRESOLVED, NONE)
        }
      }
    }

    if (evaluated != e) {
      val substituted = adjustUnitsInSum(context.substitute(ue), unit) // expression with substituted symbols
      if (substituted != ue && substituted != evaluated) {
        parts = parts :+ FormulaPart(substituted, EXPR_SUBSTITUTED)
        val depth = substituted.countTreeLeafs
        if (depth > 3 || substituted.isInstanceOf[Selection]) {
          val evaluation1 = adjustUnitsInSum((substituted match {
            case tr: Transparent => tr.children.head
            case x => x
          }) match {
            // partialy evaluated expression
            case null => substituted
            case o: Operation2 => {
              o.copy(context.evaluate(o.l), context.evaluate(o.r))
            }
            case o: OperationN => {
              o.copy(o.args.map(context.evaluate(_)): _*)
            }
            case sel: Selection => {
              context.substitute(sel.select)
            }
            case _ => context.evaluate(substituted)
          }, unit)
          if (evaluation1 != substituted && evaluation1 != evaluated) {
            parts = parts :+ FormulaPart(evaluation1, EXPR_PARTIALLY_EVALUATED)
          }
        }
      }
      val relation: FormulaPartRelation = evaluated match {
        case n: Number => if (n.isRounded) APPROXIMATELY_EQUAL else EQUAL
        case _ => EQUAL
      }
      parts = parts :+ FormulaPart(evaluated, RIGHT, relation)
    }
    Formula(parts)
  }

  def processUnresolved(symbol: Symbol, e: Expression, context: Context): Expression = {
    e match {
      case ev: EvalAt => EvalAt(ev.expr, ev.er.evaluateWithAndReturnCopy(context))
      case de: DynamicExpression => de.f()
      case _ => e
    }
  }

  def adjustUnitsInSum(e: Expression, unit: UnitOfValue): Expression = unit match {
    case EmptyUnitOfValue => e
    case u => e match {
      case s: Sum => if (s.args.exists(_.isInstanceOf[Value])) Sum(s.args.map(b => b match {
        case v: Value => v.convertTo(u)
        case _ => b
      }): _*)
      else s
      case s: min => if (s.args.exists(_.isInstanceOf[Value])) min(s.args.map(b => b match {
        case v: Value => v.convertTo(u)
        case _ => b
      }): _*)
      else s
      case s: max => if (s.args.exists(_.isInstanceOf[Value])) max(s.args.map(b => b match {
        case v: Value => v.convertTo(u)
        case _ => b
      }): _*)
      else s
      case _ => e
    }
  }

}

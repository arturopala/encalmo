package org.encalmo.calculation

import scala.collection.mutable.Map
import org.encalmo.expression._
import scala.collection.mutable

case class FormulaSet(contextId: Option[String] = None) {

    private var formulaSet = Vector[Formula]()
    private val formulaMap = mutable.Map[Expression, Formula]()

    def +=(formula: Formula): FormulaSet = {
        formulaSet = formulaSet :+ formula
        formulaMap(formula.left.expression) = formula
        this
    }

    def items: Seq[Formula] = formulaSet

    def size = items.size

    def get(expression: Expression): Option[Formula] = {
        val formulaOpt: Option[Formula] = formulaMap.get(expression)
        formulaOpt orElse {
            expression match {
                case symbol: Symbol => {
                    if (contextId.isDefined) {
                        symbol.contextId match {
                            case Some(symbolContextId) => {
                                if (!symbolContextId.contains(contextId.get)) {
                                    formulaMap.get(symbol.id(contextId.get))
                                } else {
                                    None
                                }
                            }
                            case None => formulaMap.get(symbol.id(contextId.get))
                        }
                    } else {
                        None
                    }
                }
                case _ => None
            }
        }
    }

    def getOrReckon(expression: Expression, context: ExpressionResolver, cache: ResultsCache): Formula = {
        get(expression) getOrElse {
            val formula = FormulaReckoner.reckon(expression, cache)(context)
            this += formula
            formula
        }
    }

    def contains(expression: Expression) = formulaMap.get(expression).isDefined
}

object FormulaSet {

    val echo = new FormulaSet(None) {
        override def +=(formula: Formula): FormulaSet = this

        override def get(expression: Expression): Option[Formula] = Some(Formula(Seq(FormulaPart(expression, FormulaPosition.LEFT, FormulaPartRelation.NONE))))
    }

}

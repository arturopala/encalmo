package org.encalmo.document

import org.encalmo.style.Style
import org.encalmo.calculation.{Formula, Results, Context}
import org.encalmo.expression.{TRUE, Assert,Number}

/**
 * Requirements checklist
 * @author artur.opala
 */
case class Checklist(limit: Int = 5, override val customStyleOfComponent: Option[Style] = None)(implicit val context: Context) extends DocumentComponent(customStyleOfComponent) {

    def findRequirementsFormulas(results: Results): Seq[Formula] = {
        val r = context.listRequirements.map(results.formulaSet.get).flatten
        r.toSeq.sortBy(f => f.parts(f.parts.size - 2).expression match {
            case a: Assert => a.ratio match {
                case Number(r, _) => -r.double
                case _ => Int.MinValue
            }
            case _ => Int.MinValue
        })
    }

    def findAndPartitionRequirementsFormulas(results: Results): (Seq[Formula], Seq[Formula]) = {
        findRequirementsFormulas(results) partition (f => f.result match {
            case TRUE => true
            case _ => false
        })
    }

}

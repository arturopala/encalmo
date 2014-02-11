package org.encalmo.document

import org.encalmo.style.Style
import org.encalmo.expression.Expression
import org.encalmo.calculation.Context

/**
 * Highlight given expressions
 */
class Spot(
              customStyle: Option[Style],
              styleOfResolved: Option[Style],
              styleOfEvaluated: Option[Style],
              isPrintDescription:Boolean,
              expressions:Expression*)(implicit context: Context)  extends  Evaluate(customStyle,styleOfResolved,styleOfEvaluated,isPrintDescription,expressions:_*)(context)

object Spot {

    def apply(expressions:Expression*)(implicit context: Context) = {
        new Spot(None,None,None,true,expressions:_*)(context)
    }

    def apply(customStyle:Style, expressions:Expression*)(implicit context: Context) = {
        new Evaluate(Option(customStyle),Option(customStyle),Option(customStyle),true,expressions:_*)(context)
    }

    def unapply(e:Spot) = Some(e.customStyle,e.styleOfResolved,e.styleOfEvaluated,e.context,e.expressions)
}

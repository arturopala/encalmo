package org.encalmo.document

import org.encalmo.expression.{Expression,Symbol}
import org.encalmo.calculation._
import org.encalmo.style.Style

/**
 * Requirement check
 */
class Check(
        customStyle: Option[Style],
        val isPrintDescription:Boolean,
        expressions:Expression*)(implicit context: Context)
extends BlockExpr(customStyle,expressions:_*){
	
	override def toString = "Require("+customStyle+","+isPrintDescription+","+expressions.mkString(",")+")("+context+")"
	
}

object Check {

    def apply(customStyle:Style,  expressions:Expression*)(implicit context: Context) = {
        new Check(Option(customStyle),true,expressions:_*)(context)
    }

    def apply(expressions:Expression*)(implicit context: Context) = {
        new Check(None,true,expressions:_*)(context)
    }

    def apply(isPrintDescription:Boolean, expressions:Expression*)(implicit context: Context) = {
        new Check(None,isPrintDescription,expressions:_*)(context)
    }

    def unapply(e:Check) = Some(e.customStyle,e.isPrintDescription,e.expressions,e.context)

}


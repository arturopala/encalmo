package org.encalmo.calculation

import org.encalmo.expression.Expression

case class DynamicExpression(f:()=>Expression) extends Expression
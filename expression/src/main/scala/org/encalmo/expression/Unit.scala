package org.encalmo.expression

case class ExprUnit(symb:String,exp:Int,mult:UnitMultiplier)

case class UnitMultiplier(symb:String,exp:Int)

object EmptyMultiplier extends UnitMultiplier("",1)

object EmptyExprUnit extends ExprUnit("",1,EmptyMultiplier)

object SI {
  
  object Multip {
    val None = EmptyMultiplier
  }
  
  val Meter = ExprUnit("m",1,Multip.None)
  val Newton = ExprUnit("N",1,Multip.None)
  
}
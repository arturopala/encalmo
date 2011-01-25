package org.encalmo.expression

case class UnitOfValue(symb:String,exp:Int,mult:UnitMultiplier)

case class UnitMultiplier(symb:String,exp:Int)

object EmptyMultiplier extends UnitMultiplier("",1)

object EmptyUnitOfValue extends UnitOfValue("",1,EmptyMultiplier)

object SI {
  
  object Multip {
    val None = EmptyMultiplier
  }
  
  val Meter = UnitOfValue("m",1,Multip.None)
  val Newton = UnitOfValue("N",1,Multip.None)
  
}
package org.encalmo.calculation

case class Formula(parts: Seq[FormulaPart]) {

  assert(parts.size >= 1, "Formula should have at least one item")

  def left: FormulaPart = parts.head
  def right: FormulaPart = parts.last

}

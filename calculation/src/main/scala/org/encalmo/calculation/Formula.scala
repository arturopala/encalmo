package org.encalmo.calculation

/**
 * Evaluated formula
 * @param parts
 */
case class Formula(parts: Seq[FormulaPart]) {

    assert(parts.size >= 1, "Formula should have at least one item")

    def left: FormulaPart = parts.head

    def right: FormulaPart = parts.last

    def expression = left.expression

    def result = right.expression

    lazy val face: String = parts.foldLeft("")((s,p) => s + " " + p.face)

}

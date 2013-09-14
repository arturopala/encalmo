package org.encalmo.expression

case class Assert(left: Expression, relation: Relation.Value, right: Expression) extends Expression {

    override val children = Seq(left,right)

    def face = left.face +" " + Relation.faceOf(relation) + " " + right.face

    override def eval():Expression = {
        relation match {
            case Relation.EQUAL => left === right
            case Relation.NOT_EQUAL => left <> right
            case Relation.GREATER => left > right
            case Relation.GREATER_OR_EQUAL => left >= right
            case Relation.LESS => left < right
            case Relation.LESS_OR_EQUAL => left <= right
            case _ => Unknown
        }
    }
}

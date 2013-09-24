package org.encalmo.expression

case class Assert(left:Expression, relation:Relation.Value, right:Expression, override val unit:UnitOfValue = EmptyUnitOfValue, description:Option[String] = None) extends Expression {

    override val children = Seq(left,right)

    def face = left.face +" " + Relation.faceOf(relation) + " " + right.face

    final override def map(f:Transformation):Expression = {
        val vl = left.map(f)
        val vr = right.map(f)
        if((vl eq left) && (vr eq right)) f(this) else f(copy(vl,vr))
    }

    def copy(l:Expression,r:Expression): Assert = Assert(l,relation,r,unit,description)

    final override def eval():Expression = {
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

    def unit(u: UnitOfValue): Assert = Assert(left,relation,right,u,description)
    override def ##(d: String): Assert = Assert(left,relation,right,unit,concatenate(description,Option(d)))
}

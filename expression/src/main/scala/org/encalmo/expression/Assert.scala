package org.encalmo.expression

case class Assert(left:Expression, relation:Relation.Value, right:Expression, override val unit:UnitOfValue = EmptyUnitOfValue, description:Option[String] = None, optional: Boolean = true) extends Expression {

    override val children = Seq(left,right)

    def face = left.face +" " + Relation.faceOf(relation) + " " + right.face

    final override def map(f:Transformation):Expression = {
        val vl = left.map(f)
        val vr = right.map(f)
        if((vl eq left) && (vr eq right)) f(this) else f(copyAssert(vl,vr))
    }

    def copyAssert(l:Expression,r:Expression): Assert = Assert(l,relation,r,unit,description)

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

    override def unit(unit:String):Assert = copy(unit = SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))
    override def unit(unit:UnitOfValue):Assert = copy(unit = unit)
    override def set(unit: UnitOfValue): Assert = copy(unit = unit)
    override def set(unit: String): Assert = copy(unit = SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))

    override def ##(d: String): Assert = Assert(left,relation,right,unit,concatenate(description,Option(d)))

    def ratio:Expression = (left,right) match {
        case (v1:Number,v2:Number) => relation match {
            case Relation.GREATER => (v2/v1).eval().unit(SI.percent)
            case Relation.GREATER_OR_EQUAL => (v2/v1).eval().unit(SI.percent)
            case _ => (v1/v2).eval().unit(SI.percent)
        }
        case _ => this
    }

    def required = copy(optional = false)
}

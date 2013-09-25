package org.encalmo.expression

import org.encalmo.common._

/**
 * Root type of all expressions.
 * @author artur.opala
 */
trait Expression extends TreeNode[Expression] {

    def face: String

    /**
     * Evaluates expression.
     * Subtypes should calculate possibly the simplest and numerical form of this expression.
     */
    def eval(): Expression = this

    def unit: UnitOfValue = EmptyUnitOfValue

    def unless(testCase: Case): Selection = Selection(Seq(testCase), Some(CaseExpression(this)))

    def +(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Sum(l, r) => Sum(this, l, r)
        case _ => Sum(this, e)
    }

    def -(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case _ => Diff(this, e)
    }

    def *(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Prod(l, r) => Prod(this, l, r)
        case _ => Prod(this, e)
    }

    def /(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case _ => Quot(this, e)
    }

    def %(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case _ => Modulo(this, e)
    }

    def ^(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case ZERO => ONE
        case _ => Power(this, e)
    }

    def unary_-(): Expression = this match {
        case Void => this
        case Unknown => Unknown
        case _ => Inv(this)
    }

    def +-(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Number(r, u) => this + Number(-r, u)
        case _ => this + Inv(e)
    }

    def *-(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Number(r, u) => *(Number(-r, u))
        case _ => *(Inv(e))
    }

    def /-(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Number(r, u) => /(Number(-r, u))
        case _ => /(Inv(e))
    }

    def %-(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Number(r, u) => %(Number(-r, u))
        case _ => %(Inv(e))
    }

    def ^-(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case Number(r, u) => ^(Number(-r, u))
        case _ => ^(Inv(e))
    }

    def &(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case _ => And(this, e)
    }

    def |(e: Expression): Expression = e match {
        case Void => this
        case Unknown => Unknown
        case _ => Or(this, e)
    }

    def unary_!(): Expression = this match {
        case Void => this
        case Unknown => Unknown
        case _ => Neg(this)
    }

    def <(e: Expression): Expression = Assert(this,Relation.LESS,e,this.unit)
    def <=(e: Expression): Expression = Assert(this,Relation.LESS_OR_EQUAL,e,this.unit)
    def >(e: Expression): Expression = Assert(this,Relation.GREATER,e,this.unit)
    def >=(e: Expression): Expression = Assert(this,Relation.GREATER_OR_EQUAL,e,this.unit)
    def <>(e: Expression): Expression = Assert(this,Relation.NOT_EQUAL,e,this.unit)
    def ===(e: Expression): Expression = Assert(this,Relation.EQUAL,e,this.unit)

    def printable = true

    final def nounit: Expression = NoUnit(this)
    def unit(unit: UnitOfValue): Expression = WithUnit(this, unit)
    def unit(unit: String): Expression = WithUnit(this, SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))
    def set(unit: UnitOfValue): Expression = SetUnit(this, unit)
    def set(unit: String): Expression = SetUnit(this, SI(unit).getOrElse(SimpleUnitOfValue(UnitOfValueName(unit),0,1,SI)))

    def ##(description:String): Expression = Annotated(this,description)
    def ##(description:Option[String]): Expression = Annotated(this,description)

    protected def concatenate(args: Option[String]*): Option[String] = {
        args.fold(None)((ac,v) => ac match {
            case None => v
            case some => v match {
                case None => some
                case Some(d) => some.map(_ +" "+d)
            }
        })
    }

}

object Expression {

    def simplify(e: Expression): Expression = {
        e.mapAll(Transformations.simplifyQuot)
    }

}
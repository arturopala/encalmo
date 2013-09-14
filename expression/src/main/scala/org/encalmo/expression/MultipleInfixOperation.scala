package org.encalmo.expression

/**
 * Multi-argument (with number of arguments usually greater than 2) infix operation trait
 * @author artur.opala
 */
trait MultipleInfixOperation extends OperationN with PrimitiveOperation {

    override def face = "(" + args.foldLeft("")((s,e) => if(!s.isEmpty) s + " " + operator.name + " " + e.face else e.face) + ")"

}
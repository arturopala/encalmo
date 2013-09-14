package org.encalmo.expression

/**
 * Definition is a link between symbol and other expression
 * @author artur.opala
 *
 */
case class Definition(s:Symbol,e:Expression) extends Expression {

    override def face = s.face + " -> " + e.face
    override def map(f:Transformation):Expression = e.map(f)
  
}
package org.encalmo.common

/**
 * Tree structure node
 * @author artur.opala
 */
case class Node[A](parent:Node[A], element:A, position:Int = 0) {
	
	lazy val level:Int = parent match {
		case null => 0
		case _ => parent.level + 1
	}
	
	lazy val coordinate:Seq[Int] = parent match {
		case null => Seq(position)
		case _ => parent.coordinate :+ position
	}
	
}
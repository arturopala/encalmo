package org.encalmo.common

/**
 * Trait of objects traversable with {@link TreeVisitor}
 * @author artur.opala
 */
trait Visitable[A<:AnyRef] {
	
	/**
	 * Main function to implement.
	 * Visits internal structure of the object with visitor.
	 * @param traveler traveler
	 */
	def visit(visitor:TreeVisitor[A], parent:Node[A] = null, position:Int = 0):Unit
	
	/**
	 * Select element from tree 
	 * @param coordinate - node coordinate, zero based
	 * @return found element option
	 */
	final def select(coordinate:Int*):Option[A] = {
		val t = new Selector[A](coordinate:_*)
		try {
			this.visit(visitor = t);
		}
		catch {
			case e:EndOfVisitException => 
			case e:RuntimeException => throw e
		}
		t.result 
	}
	
	/**
	 * Dumps tree structure to the console
	 */
	final def dumpTreeToConsole = {
		this.visit(visitor = DumpTreeVisitor);
	}
	
	object DumpTreeVisitor extends TreeVisitor[A] {
		override def onEnter(node:Node[A]):Unit = {
			for(i <- 1 to node.level){
				Console.print("   ")
			}
			Console.print(node.coordinate.mkString("(", ",", ")"))
			Console.print(" ")
			Console.println(node.element.getClass)
		}
	}

}


sealed class Selector[A<:AnyRef](coordinate:Int*) extends TreeVisitor[A] {
	
	var result:Option[A] = None
	
	override def onEnter(node:Node[A]):Unit = {
		val nc = node.coordinate
		if(nc.sameElements(coordinate)){
			result = Some(node.element)
			throw new EndOfVisitException
		}
	}
}

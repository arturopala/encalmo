package org.encalmo.common

/**
 * Interface of travelable tree-like strutures
 * @author artur.opala
 */
trait Travelable[A] {
	
	/**
	 * Main function to implement.
	 * Travels internal structure of the tree
	 * @param traveler traveler
	 */
	def travel(parent:Node[A] = null, traveler:Traveler[A], position:Int = 0):Unit
	
	/**
	 * Selects element from tree 
	 * @param coordinate - node coordinate, zero based
	 * @return found element option
	 */
	final def select(coordinate:Int*):Option[A] = {
		val t = new SelectTraveler[A](coordinate:_*)
		try {
			this.travel(traveler = t);
		}
		catch {
			case e:TravelBreakException => 
			case e:RuntimeException => throw e
		}
		
		t.result 
	}
	
	private class SelectTraveler[A](coordinate:Int*) extends Traveler[A] {
		
		var result:Option[A] = None
		
		override def onEnter(node:Node[A]):Unit = {
			val nc = node.coordinate
			if(nc.sameElements(coordinate)){
				result = Some(node.element)
				throw new TravelBreakException
			}
		}
	}
	
	/**
	 * Dumps tree structure to the console
	 */
	final def dumpTreeToConsole = {
		this.travel(traveler = DumpTreeTraveler);
	}
	
	private object DumpTreeTraveler extends Traveler[A] {
		override def onEnter(node:Node[A]):Unit = {
			for(i <- 1 to node.level){
				Console.print("   ")
			}
			Console.print(node.coordinate.mkString("(", ",", ")"))
			Console.print(" ")
			Console.println(node.element)
		}
	}

}
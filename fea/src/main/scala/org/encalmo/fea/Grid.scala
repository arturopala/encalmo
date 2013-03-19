package org.encalmo.fea

/** Grid is a provider of node's coordinates */
trait Grid {
  
	/** Returns coordinates of [x,y,z] node of grid */
	def apply(x:Int,y:Int,z:Int):Vector

}

/** Grid's factory */
object Grid {
  
	private type Matrix = Seq[Seq[Double]]
	
	private def multiply (x:Int,y:Int,z:Int,m:Matrix):Vector = {
		Vector(x*m(0)(0)+y*m(0)(1)+z*m(0)(2),x*m(1)(0)+y*m(1)(1)+z*m(1)(2),x*m(2)(0)+y*m(2)(1)+z*m(2)(2))
	}
  
	def fromDiagonal(diagonal:Vector, base:Vector = ZeroVector) = new Grid {
		override def apply(x:Int,y:Int,z:Int):Vector = base + (diagonal ** (x,y,z))
	}
	
	def fromMatrix(matrix:Matrix, base:Vector = ZeroVector) = new Grid {
		override def apply(x:Int,y:Int,z:Int):Vector = base + multiply(x,y,z,matrix)
	}
  
}
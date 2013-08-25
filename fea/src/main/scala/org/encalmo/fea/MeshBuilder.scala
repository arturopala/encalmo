package org.encalmo.fea

/** Mesh builder */
object MeshBuilder {
    
    def plate20(grid:Seq[Seq[Node]],x:Int,y:Int):Plate20 = {
        val nodes = IndexedSeq(grid(x+0)(y+0),grid(x+2)(y+0), grid(x+2)(y+2),grid(x+0)(y+2),grid(x+1)(y+0),grid(x+2)(y+1),grid(x+1)(y+2),grid(x+0)(y+1))
        Plate20(nodes)
    }
    
    def buildRectanglePlate20(w:Double,h:Double,mx:Int,my:Int):Mesh[Plate20] = {
        buildRectanglePlate[Plate20](2,plate20)(w,h,mx,my)
    }
    
    def plate19(grid:Seq[Seq[Node]],x:Int,y:Int):Plate19 = {
        val nodes = IndexedSeq(
                grid(x+0)(y+0),grid(x+0)(y+1),grid(x+0)(y+2),grid(x+0)(y+3),
                grid(x+1)(y+0),grid(x+1)(y+1),grid(x+1)(y+2),grid(x+1)(y+3),
                grid(x+2)(y+0),grid(x+2)(y+1),grid(x+2)(y+2),grid(x+2)(y+3),
                grid(x+3)(y+0),grid(x+3)(y+1),grid(x+3)(y+2),grid(x+3)(y+3)
        )
        Plate19(nodes)
    }
    
    def buildRectanglePlate19(w:Double,h:Double,mx:Int,my:Int):Mesh[Plate19] = {
        buildRectanglePlate[Plate19](3,plate19)(w,h,mx,my)
    }
    
    def buildRectanglePlate[A <: FiniteElement](s:Int, plate:(Seq[Seq[Node]],Int,Int) => A)(w:Double,h:Double,mx:Int,my:Int):Mesh[A] = {
    	val cw:Int = mx*s; val ch:Int = my*s
    	val grid  = Grid.fromDiagonal(Vector(w/cw,h/ch,0d))
        val nodeGrid:Seq[Seq[Node]] = buildNodeGrid2D(grid,cw,ch,Node(grid,0,0,0,3),1)
        val r = (0 to (cw - s) by s) flatMap (x => (0 to (ch - s) by s) map (y => (x, y)))
        val elements = r.map(p => plate(nodeGrid,p._1,p._2))
        Mesh[A](elements)
    }
    
    def buildNodeGrid2D(grid:Grid,width:Int,length:Int,base:Node,d:Int):IndexedSeq[Seq[Node]] = {
        buildNodeGrid1D(grid,width,base,d).map[IndexedSeq[Node],IndexedSeq[IndexedSeq[Node]]](n => buildNodeGrid1D(grid,length,n,d+1))
    }
    
    def buildNodeGrid1D(grid:Grid,length:Int,base:Node,d:Int):IndexedSeq[Node] = {
        (1 to length).map[Node,IndexedSeq[Node]](l => Node(grid,base.gx+l*ifDirection(d,1),base.gy+l*ifDirection(d,2),base.gz+l*ifDirection(d,3),(base.position-1)+oneIfLast(l,length))).+:(base)
    }
    
    def ifDirection(d1:Int,d2:Int):Int = if(d1==d2) 1 else 0
    
    def oneIfLast(x:Int,max:Int):Int = x match {case i if i==max => 1; case _ => 0}
    
}
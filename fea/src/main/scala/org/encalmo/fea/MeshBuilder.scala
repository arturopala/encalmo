package org.encalmo.fea

/** Mesh builder */
object MeshBuilder {
    
    def plate20(grid:Seq[Seq[Node]],x:Int,y:Int):Plate20 = {
        val nodes = IndexedSeq(grid(x+0)(y+0),grid(x+2)(y+0), grid(x+2)(y+2),grid(x+0)(y+2),grid(x+1)(y+0),grid(x+2)(y+1),grid(x+1)(y+2),grid(x+0)(y+1))
        Plate20(nodes)
    }
    
    def buildRectanglePlate20(w:Double,h:Double,mx:Int,my:Int):Mesh[Plate20] = {
        buildRectanglePlate[Plate20](2,plate20 _)(w,h,mx,my)
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
        buildRectanglePlate[Plate19](3,plate19 _)(w,h,mx,my)
    }
    
    def buildRectanglePlate[A <: FiniteElement](s:Int, plate:(Seq[Seq[Node]],Int,Int) => A)(w:Double,h:Double,mx:Int,my:Int):Mesh[A] = {
        val cx:Int = mx*s; val cy:Int = my*s
        val base:Node = Node(0d,0d,0d,3)
        val grid:Seq[Seq[Node]] = buildNodeGrid2D(base,(w/cx,0d,0d),(0d,h/cy,0d),cx,cy)
        val r = ((0 to (cx-s) by s) flatMap (x => (0 to (cy-s) by s) map (y => (x,y))))
        val elements = r.map(p => plate(grid,p._1,p._2))
        Mesh[A](elements)
    }
    
    def buildNodeGrid2D(base:Node,v1:Vector,v2:Vector,c1:Int,c2:Int):IndexedSeq[Seq[Node]] = {
        buildNodeGrid1D(base,v1,c1).map[IndexedSeq[Node],IndexedSeq[IndexedSeq[Node]]](buildNodeGrid1D(_,v2,c2))
    }
    
    def buildNodeGrid1D(base:Node,v:Vector,size:Int):IndexedSeq[Node] = {
        (1 to size).map[Node,IndexedSeq[Node]](x => Node(v,(base.position-1)+oneIfLast(x,size))).scanLeft[Node,IndexedSeq[Node]](base)((l,r) => r join l)
    }
    
    def oneIfLast(x:Int,max:Int):Int = x match {case i if i==max => 1; case _ => 0}
    
}
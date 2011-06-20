package org.encalmo.fea

/** Mesh builder */
object MeshBuilder {
    
    def buildPlateStructureFromRectangle(w:Double,h:Double,mx:Int,my:Int):Mesh[Plate20] = {
        val cx:Int = mx*2; val cy:Int = my*2
        val base:Node = Node(0d,0d,0d,3)
        val grid:Seq[Seq[Node]] = buildNodeGrid(base,(w/cx,0d,0d),(0d,h/cy,0d),cx,cy)
        def plate(x:Int,y:Int):Plate20 = {
            val nodes = Seq(grid(x+0)(y+0),grid(x+2)(y+0), grid(x+2)(y+2),grid(x+0)(y+2),grid(x+1)(y+0),grid(x+2)(y+1),grid(x+1)(y+2),grid(x+0)(y+1))
            Plate20(nodes)
        }
        val r = ((0 to (cx-2) by 2) flatMap (x => (0 to (cy-2) by 2) map (y => (x,y))))
        val elements = r.map(p => plate(p._1,p._2))
        Mesh[Plate20](elements)
    }
    
    def buildNodeGrid(base:Node,v1:Vector,v2:Vector,c1:Int,c2:Int):Seq[Seq[Node]] = {
        buildNodeSeq(base,v1,c1).map(buildNodeSeq(_,v2,c2))
    }
    
    def buildNodeSeq(base:Node,v:Vector,size:Int):Seq[Node] = {
        (1 to size).map(x => Node(v,(base.position-1)+oneIfLast(x,size))).scanLeft[Node,Seq[Node]](base)((l,r) => r join l)
    }
    
    def oneIfLast(x:Int,max:Int):Int = x match {case i if i==max => 1; case _ => 0}
    
}
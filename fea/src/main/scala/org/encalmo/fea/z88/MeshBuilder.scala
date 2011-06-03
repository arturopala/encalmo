package org.encalmo.fea.z88

import org.encalmo.fea.Vector

object MeshBuilder {
    
    def buildPlateStructureFromRectangle(w:Double,h:Double,t:Double,mat:Material,mx:Int,my:Int):Structure[Plate20] = {
        
        val cx = mx*2
        val cy = my*2
        
        val base = Node((0d,0d,0d))
        val grid = buildNodeGrid(base,(w/cx,0d,0d),(0d,h/cy,0d),cx,cy)
        
        def plate(x:Int,y:Int) = Plate20(
            Seq(grid(x+0)(y+0),
                grid(x+2)(y+0),
                grid(x+2)(y+2),
                grid(x+0)(y+2),
                grid(x+1)(y+0),
                grid(x+2)(y+1),
                grid(x+1)(y+2),
                grid(x+0)(y+1))
                ,t,mat)
        val r = ((0 to (cx-2) by 2) flatMap (x => (0 to (cy-2) by 2).map(y => (x,y))))
        val s = r.map(p => plate(p._1,p._2))
                
        Structure[Plate20](s)
    }
    
    def buildNodeSeq(base:Node,v:Vector,c:Int):Seq[Node] = {
        (1 to c).map(x => Node(v)).scanLeft[Node,Seq[Node]](base)((l,r) => r join l)
    }
    
    def buildNodeGrid(base:Node,v1:Vector,v2:Vector,c1:Int,c2:Int):Seq[Seq[Node]] = {
        buildNodeSeq(base,v1,c1).map(buildNodeSeq(_,v2,c2))
    }
    
}
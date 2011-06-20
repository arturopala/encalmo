package org.encalmo.fea.z88

import org.encalmo.fea.Vector
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import java.io.File
import scalax.file.{Path}

/**
 * Z88 plate structure generation test
 * @author artur.opala
 */
class Z88PlateUnitTest {
    
    val concrete = Material(30000000d,0.3)
    def materialFx(e:FiniteElement):Material = concrete
    def thicknessFx(e:FiniteElement):Double = 0.2+e.center.y/50
    def surfaceLoadFx(e:FiniteElement):Option[Seq[Option[Double]]] = Some(Seq(Some(e.center.y*10d)))
    def forceFx(n:Node):Option[Seq[Option[Double]]] = None
    
    @Test def test1 = {
       val base = Node((0d,0d,0d))
	   val s:Seq[Node] = MeshBuilder.buildNodeSeq(base,(1d,0d,0d),10)
	   0 until 10 foreach (i => assertEquals(s(i+1).base.get,s(i)))
    }
    
    @Test def test2 = {
        val w:Double = 10
        val h:Double = 15
        def displacementFx(n:Node):Option[Seq[Option[Double]]] = Some(Seq(n match {
            case n if n.onEdge => n.y match {
                case x if x==h => None
                case _ => Some(0d)
            }
            case _ => None
        }))
	    val mesh:Mesh[Plate20] = MeshBuilder.buildPlateStructureFromRectangle(w,h,10,10, materialFx _,thicknessFx _,surfaceLoadFx _, forceFx _, displacementFx _)
	    mesh.elements foreach (p => {
		    assertEquals(p.nodes(0).x, p.nodes(3).x, 0);
		    assertEquals(p.nodes(1).x, p.nodes(2).x, 0);
		    assertEquals(p.nodes(4).x, p.nodes(6).x, 0);
		    assertEquals(p.nodes(0).x, p.nodes(7).x, 0);
		    assertEquals(p.nodes(0).x, p.nodes(7).x, 0);
		    assertEquals(p.nodes(5).y, p.nodes(7).y, 0);
		    assertEquals(p.nodes(2).y, p.nodes(3).y, 0)
	    })
	    val p = Z88Project(mesh, Path("target/z88"))
	    p.createInputFiles
    }

}
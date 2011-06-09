package org.encalmo.fea.z88
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
    
    @Test def test1 = {
       val base = Node((0d,0d,0d))
	   val s:Seq[Node] = MeshBuilder.buildNodeSeq(base,(1d,0d,0d),10)
	   0 until 10 foreach (i => assertEquals(s(i+1).base.get,s(i)))
    }
    
    @Test def test2 = {
        val r:Int = 15
	    val mat = Material(205000000000d,0.3)
	    val mesh:Mesh[Plate20] = MeshBuilder.buildPlateStructureFromRectangle(r,r,0.2,mat,r/2,r/2)
	    0 until (r/2)*(r/2) foreach (i => {
	        val p = mesh.elements(i);
		    assertEquals(p.nodes(0).c.x, p.nodes(3).c.x, 0);
		    assertEquals(p.nodes(1).c.x, p.nodes(2).c.x, 0);
		    assertEquals(p.nodes(4).c.x, p.nodes(6).c.x, 0);
		    assertEquals(p.nodes(0).c.x, p.nodes(7).c.x, 0);
		    assertEquals(p.nodes(0).c.x, p.nodes(7).c.x, 0);
		    assertEquals(p.nodes(5).c.y, p.nodes(7).c.y, 0);
		    assertEquals(p.nodes(2).c.y, p.nodes(3).c.y, 0)
	    })
	    val p = Z88Project(mesh, Path("target/z88"))
	    p.createInputFiles
    }

}
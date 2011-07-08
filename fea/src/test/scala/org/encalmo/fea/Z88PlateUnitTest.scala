package org.encalmo.fea

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
    
    val t = 0.4d
    val E = 10E9d
    val v = 1/6d
    val q = 10E3d
    
    val concrete = Material(E,v)
    
    @Test def testGrid1D = {
       val base = Node(0d,0d,0d,3)
	   val s:Seq[Node] = MeshBuilder.buildNodeGrid1D(base,(1d,0d,0d),10)
	   0 until 10 foreach (i => {
	         assertEquals(s(i+1).base.get,s(i))
	         i match {
	             case 0 => assertEquals(3,s(i).position)
	             case 10 => assertEquals(3,s(i).position)
	             case _ => assertEquals(2,s(i).position)
	         }
	   })
    }
    
    @Test def testLoadZ88O2:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path("fea/src/test/resources"))
       val s1 = p.readOutputFile_Z88O2
       assertTrue(s1.size>0)
    }
    
    @Test def testLoadZ88O3:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path("fea/src/test/resources"))
       val s1 = p.readOutputFile_Z88O3(0)
       assertTrue(s1.size>0)
       val s2 = p.readOutputFile_Z88O3(1)
       assertTrue(s2.size>0)
    }
    
    @Test def testLoadZ88O4:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path("fea/src/test/resources"))
       val s = p.readOutputFile_Z88O4
       assertTrue(s.size>0)
    }
    
    @Test def testPlate20 = {
        val w:Double = 10
        val h:Double = 10
        def materialFx(e:Plate20):Material = concrete
        def thicknessFx(e:Plate20):Double = t/*+e.center.y/50*/
        def surfaceLoadFx(e:Plate20):OptDoubleSeq = OptDoubleSeq(10E3)/*Some(Seq(Some(e.center.y*2d)))*/
        def forceFx(n:Node):Option[NodeForce] = None
        def displacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && v==h) => None
                case _ => Some(NodeDisplacement.forPlate(0d,0d,0d))
            }
            case _ => None
        }
	    val mesh:Mesh[Plate20] = MeshBuilder.buildRectanglePlate20(w,h,10,10)
	    // assert valid nodes of elements
	    mesh.elements foreach (p => {
		    assertEquals(p.nodes(0).x, p.nodes(3).x, 0)
		    assertEquals(p.nodes(1).x, p.nodes(2).x, 0)
		    assertEquals(p.nodes(4).x, p.nodes(6).x, 0)
		    assertEquals(p.nodes(0).x, p.nodes(7).x, 0)
		    assertEquals(p.nodes(0).x, p.nodes(7).x, 0)
		    assertEquals(p.nodes(5).y, p.nodes(7).y, 0)
		    assertEquals(p.nodes(2).y, p.nodes(3).y, 0)
	    })
        val loadCase:LoadCase[Plate20] = LoadCase[Plate20](mesh,materialFx _,thicknessFx _,surfaceLoadFx _,forceFx _,displacementFx _)
	    val p:Z88Project[Plate20] = Z88Project(Plate20Type, loadCase, Path("target/z88_20"))
	    p.createInput
	    p.runCalculations()
	    val loadResult:LoadResults[Plate20] = p.readOutput
	    // assert boundary conditions
	    loadResult.nodeResults.foreach(nr => {
	        nr.conditions match {
	            case Some(nc) => {
	                assertEquals(nr.explain,nc.displacement.get,nr.displacement)
	            }
	            case _ =>
	        }
	    })
	    val nr1:NodeResult = loadResult.maxDZ
	    val D = ((concrete.E*t*t*t)/(12*(1-v*v)))
	    Console.println("D="+D)
	    Console.println("a/b="+(h/w))
	    val qb4D = ((q*w*w*w*w)/D)
	    Console.println("qb4/D="+qb4D)
	    Console.println("k="+(nr1.displacement.dz.get/qb4D))
	    Console.println("Max D1 node: "+nr1.explain)
	    val nr2:NodeResult = loadResult.minDZ
	    Console.println("Min D1 node: "+nr2.explain)
    }
    
    @Test def testPlate19 = {
        val w:Double = 10
        val h:Double = 10
        def materialFx(e:Plate19):Material = concrete
        def thicknessFx(e:Plate19):Double = t/*+e.center.y/50*/
        def surfaceLoadFx(e:Plate19):OptDoubleSeq = OptDoubleSeq(10E3)/*Some(Seq(Some(e.center.y*2d)))*/
        def forceFx(n:Node):Option[NodeForce] = None
        def displacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && v==h) => None
                case _ => Some(NodeDisplacement.forPlate(0d,0d,0d))
            }
            case _ => None
        }
        val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(w,h,10,10)
        // assert valid nodes of elements
        mesh.elements foreach (p => {
            assertEquals(p.nodes(0).x, p.nodes(1).x, 0)
            assertEquals(p.nodes(1).x, p.nodes(2).x, 0)
            assertEquals(p.nodes(2).x, p.nodes(3).x, 0)
            assertEquals(p.nodes(4).x, p.nodes(5).x, 0)
            assertEquals(p.nodes(5).x, p.nodes(6).x, 0)
            assertEquals(p.nodes(6).x, p.nodes(7).x, 0)
            assertEquals(p.nodes(8).x, p.nodes(9).x, 0)
            assertEquals(p.nodes(9).x, p.nodes(10).x, 0)
            assertEquals(p.nodes(10).x, p.nodes(11).x, 0)
            assertEquals(p.nodes(12).x, p.nodes(13).x, 0)
            assertEquals(p.nodes(13).x, p.nodes(14).x, 0)
            assertEquals(p.nodes(14).x, p.nodes(15).x, 0)
            assertEquals(p.nodes(0).y, p.nodes(4).y, 0)
            assertEquals(p.nodes(1).y, p.nodes(5).y, 0)
            assertEquals(p.nodes(2).y, p.nodes(6).y, 0)
            assertEquals(p.nodes(3).y, p.nodes(7).y, 0)
            assertEquals(p.nodes(4).y, p.nodes(8).y, 0)
            assertEquals(p.nodes(5).y, p.nodes(9).y, 0)
            assertEquals(p.nodes(6).y, p.nodes(10).y, 0)
            assertEquals(p.nodes(7).y, p.nodes(11).y, 0)
            assertEquals(p.nodes(8).y, p.nodes(12).y, 0)
            assertEquals(p.nodes(9).y, p.nodes(13).y, 0)
            assertEquals(p.nodes(10).y, p.nodes(14).y, 0)
            assertEquals(p.nodes(11).y, p.nodes(15).y, 0)
        })
        val loadCase:LoadCase[Plate19] = LoadCase[Plate19](mesh,materialFx _,thicknessFx _,surfaceLoadFx _,forceFx _,displacementFx _)
        val p:Z88Project[Plate19] = Z88Project(Plate19Type, loadCase, Path("target/z88_19"))
        p.createInput
        //p.runCalculations()
        val loadResult:LoadResults[Plate19] = p.readOutput
        // assert boundary conditions
        loadResult.nodeResults.foreach(nr => {
            nr.conditions match {
                case Some(nc) => {
                    assertEquals(nc.displacement.get,nr.displacement)
                }
                case _ =>
            }
        })
        val nr1:NodeResult = loadResult.maxDZ
        val D = ((concrete.E*t*t*t)/(12*(1-v*v)))
        Console.println("D="+D)
        Console.println("a/b="+(h/w))
        val qb4D = ((q*w*w*w*w)/D)
        Console.println("qb4/D="+qb4D)
        Console.println("k="+(nr1.displacement.dz.get/qb4D))
        Console.println("Max Z node: "+nr1.explain)
        val nr2:NodeResult = loadResult.maxMXX
        Console.println("Max MXX node: "+nr2.explain)
        val nr3:NodeResult = loadResult.minMXX
        Console.println("Min MXX node: "+nr3.explain)
        val nr4:NodeResult = loadResult.maxMYY
        Console.println("Max MYY node: "+nr4.explain)
        val nr5:NodeResult = loadResult.minMYY
        Console.println("Min MYY node: "+nr5.explain)
        val nr6:NodeResult = loadResult.maxFZ
        Console.println("Max FZ node: "+nr6.explain)
        val nr7:NodeResult = loadResult.minFZ
        Console.println("Min FZ node: "+nr7.explain)
        
    }

}
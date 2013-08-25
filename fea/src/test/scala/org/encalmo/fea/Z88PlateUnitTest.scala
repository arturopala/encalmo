package org.encalmo.fea

import org.junit.{Ignore, Test}
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
       val width:Int = 10
       val diagonal = Vector(1d,1d,1d)
       val grid  = Grid.fromDiagonal(diagonal)
       val base = Node(grid,0,0,0,3)
	   val s:Seq[Node] = MeshBuilder.buildNodeGrid1D(grid,width, base, 1)
	   0 to width foreach (i => {
	         i match {
	             case x if x==0 || x==width => assertEquals(3,s(i).position)
	             case _ => assertEquals(2,s(i).position)
	         }
	         assertTrue(s(i).coordinates == s(i).location*diagonal)
	   })
    }
    
    @Test def testGrid2D = {
       val width = 10
       val length = 15
       val diagonal = Vector(1d,1d,1d)
       val grid  = Grid.fromDiagonal(diagonal)
       val base = Node(grid,0,0,0,3)
	   val s:IndexedSeq[Seq[org.encalmo.fea.Node]] = MeshBuilder.buildNodeGrid2D(grid,width,length, base, 1)
	   for (i <- 0 to width; j <- 0 to length) {
	     (i,j) match {
             case (x,y) if (x==0 || x==width) && (y==0 || y==length) => assertEquals(i+","+j,3,s(i)(j).position)
             case (x,y) if (x==0 || x==width) && y>0 && y<length => assertEquals(i+","+j,2,s(i)(j).position)
             case (x,y) if (y==0 || y==length) && x>0 && x<width => assertEquals(i+","+j,2,s(i)(j).position)
             case _ => assertEquals(i+","+j,1,s(i)(j).position)
         }
	     assertTrue(s(i)(j).coordinates == s(i)(j).location*diagonal)
	  }
}
    
    @Test def testLoadZ88O2:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path.fromString("src/test/resources"))
       val s1 = p.readOutputFile_Z88O2
       assertTrue(s1.size>0)
    }
    
    @Test def testLoadZ88O3:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path.fromString("src/test/resources"))
       val s1 = p.readOutputFile_Z88O3(0)
       assertTrue(s1.size>0)
       val s2 = p.readOutputFile_Z88O3(1)
       assertTrue(s2.size>0)
    }
    
    @Test def testLoadZ88O4:Unit = {
       val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(10d,10d,10,10)
       val p:Z88Project[Plate19] = Z88Project(Plate19Type, LoadCase[Plate19](mesh,null,null,null,null,null), Path.fromString("src/test/resources"))
       val s = p.readOutputFile_Z88O4
       assertTrue(s.size>0)
    }
    
    @Test def testPlate20:Unit = {
        val w:Double = 10
        val h:Double = 10
        def materialFx(e:Plate20):Material = concrete
        def thicknessFx(e:Plate20):Double = t/*+e.center.y/50*/
        def surfaceLoadFx(e:Plate20):OptDoubleSeq = OptDoubleSeq(10E3)/*Some(Seq(Some(e.center.y*2d)))*/
        def forceFx(n:Node):Option[NodeForce] = None
        def displacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && v==h) => None
                case _ => Some(NodeDisplacement(Some(0d),Some(0d),Some(0d),Some(0d),Some(0d),Some(0d)))
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
        val loadCase:LoadCase[Plate20] = LoadCase[Plate20](mesh,materialFx,thicknessFx,surfaceLoadFx,forceFx,displacementFx)
	    val p:Z88Project[Plate20] = Z88Project(Plate20Type, loadCase, Path.fromString("target/z88_20"))
	    p.createInput
	    p.runCalculations()
	    val loadResult:LoadResults[Plate20] = p.readOutput
	    // assert boundary conditions
        loadResult.nodeResults.values.foreach(nr => {
            nr.conditions match {
                case Some(nc) => {
                    if(!(nr.displacement.meets(nc.displacement.get))){
                        Console.println("Result of node #"+nr.node.no+" meets no required conditions: "+nr.explain)
                    }
                }
                case _ =>
            }
        })
        Console.println(loadResult.report)
        val nr1:NodeResult = loadResult.maxDZ
        val D = ((concrete.E*t*t*t)/(12*(1-v*v)))
        Console.println("D="+D)
        Console.println("a/b="+(h/w))
        val qb4D = ((q*w*w*w*w)/D)
        val qb2 = q*w*w
        val qa2 = q*h*h
        Console.println("qb4/D="+qb4D)
        Console.println("qb2="+qb2)
        val kdz = nr1.displacement.dz.get/qb4D
        Console.println("kdz="+kdz)
        /*assertEquals(akdz,kdz,Vector.ACCURACY)
        val nr2:NodeResult = loadResult.maxMXXspan
        if(nr2!=null){
            val kmxx1 = nr2.stress.get.mxx.get/qb2
            Console.println("kmxx(H1)="+kmxx1)
            assertEquals(akmxx1,kmxx1,Vector.ACCURACY)
        }
        val nr3:NodeResult = loadResult.minMXXsupp
        if(nr3!=null){
            val kmxx2 = nr3.stress.get.mxx.get/qa2
            Console.println("kmxx(C)="+kmxx2)
            assertEquals(akmxx2,kmxx2,Vector.ACCURACY)
        }
        val nr5:NodeResult = loadResult.minMYYsupp
        if(nr5!=null){
            val kmyy1 = nr5.stress.get.myy.get/qa2
            Console.println("kmyy(H2)="+kmyy1)
            assertEquals(akmyy1,kmyy1,Vector.ACCURACY)
        }*/
    }
    
    /** 3-side rigid mounting, constant surface load */
    @Test def testPlate19a:Unit = {
        val w:Double = 10
        val h:Double = 10
        val r:Int = 10
        def materialFx(e:Plate19):Material = concrete
        def thicknessFx(e:Plate19):Double = t
        def surfaceLoadFx(e:Plate19):OptDoubleSeq = OptDoubleSeq(10E3)
        def nodeForceFx(n:Node):Option[NodeForce] = None
        def nodeDisplacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && Vector.equals(v,h)) => None
                case _ => Some(NodeDisplacement(Some(0d),Some(0d),Some(0d),Some(0d),Some(0d),Some(0d)))
            }
            case _ => None
        }
        testPlate19(w,h,r,materialFx,thicknessFx,surfaceLoadFx,nodeForceFx,nodeDisplacementFx, 0.00283255, 0.0425, -0.08649, -0.05587)
    }
    
    @Test def testPlate19b:Unit = {
        val w:Double = 10
        val h:Double = 10
        val r:Int = 10
        def materialFx(e:Plate19):Material = concrete
        def thicknessFx(e:Plate19):Double = t
        def surfaceLoadFx(e:Plate19):OptDoubleSeq = OptDoubleSeq(10E3)
        def nodeForceFx(n:Node):Option[NodeForce] = None
        def nodeDisplacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && Vector.equals(v,h)) => None
                case _ => Some(NodeDisplacement(None,None,Some(0d),None,None,None))
            }
            case _ => None
        }
        testPlate19(w,h,r,materialFx,thicknessFx,surfaceLoadFx,nodeForceFx,nodeDisplacementFx, 0.012019, 0.1089, -0.01067, -0.01065)
    }

    @Ignore
    @Test def testPlate19c:Unit = {
        val w:Double = 10
        val h:Double = 10
        val r:Int = 10
        def materialFx(e:Plate19):Material = concrete
        def thicknessFx(e:Plate19):Double = t
        def surfaceLoadFx(e:Plate19):OptDoubleSeq = OptDoubleSeq(10E3)
        def nodeForceFx(n:Node):Option[NodeForce] = None
        def nodeDisplacementFx(n:Node):Option[NodeDisplacement] = n match {
            case n if (n.onEdge || n.inCorner) => n.y match {
                case v if (n.onEdge && (Vector.equals(v,h) || Vector.equals(v,0))) => None
                case _ => Some(NodeDisplacement(Some(0d),Some(0d),Some(0d),Some(0d),Some(0d),Some(0d)))
            }
            case _ => None
        }
        testPlate19(w,h,r,materialFx,thicknessFx,surfaceLoadFx,nodeForceFx,nodeDisplacementFx, 0.012019, 0.1089, -0.01067, -0.01065)
    }
    
    /** Common Plate19 test's code */
    private def testPlate19( 
            w:Double, 
            h:Double, 
            r:Int, 
            /** Material properties function */
            materialFx:Plate19=>Material,
            /** Element thickness function, if applicable */
            thicknessFx:Plate19=>Double,
            /** Surface load function, if applicable */
            surfaceLoadFx:Plate19=>OptDoubleSeq,
            /** Established node's force function */
            nodeForceFx:Node=>Option[NodeForce],
            /** Established node's displacement function */
            nodeDisplacementFx:Node=>Option[NodeDisplacement],
            akdz:Double,
            akmxx1:Double,
            akmxx2:Double,
            akmyy1:Double
     ):LoadResults[Plate19] = {
        
        val mesh:Mesh[Plate19] = MeshBuilder.buildRectanglePlate19(w,h,r,r)
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
        val loadCase:LoadCase[Plate19] = LoadCase[Plate19](mesh,materialFx,thicknessFx,surfaceLoadFx,nodeForceFx,nodeDisplacementFx)
        val p:Z88Project[Plate19] = Z88Project(Plate19Type, loadCase, Path.fromString("target/z88_19"))
        p.createInput
        p.runCalculations()
        val loadResult:LoadResults[Plate19] = p.readOutput
        // assert boundary conditions
        loadResult.nodeResults.values.foreach(nr => {
            nr.conditions match {
                case Some(nc) => {
                    if(!(nr.displacement.meets(nc.displacement.get))){
                        Console.println("Result of node #"+nr.node.no+" meets no required conditions: "+nr.explain)
                    }
                }
                case _ =>
            }
        })
        Console.println(loadResult.report)
        val nr1:NodeResult = loadResult.maxDZ
        val D = ((concrete.E*t*t*t)/(12*(1-v*v)))
        Console.println("D="+D)
        Console.println("a/b="+(h/w))
        val qb4D = ((q*w*w*w*w)/D)
        val qb2 = q*w*w
        val qa2 = q*h*h
        Console.println("qb4/D="+qb4D)
        Console.println("qb2="+qb2)
        val kdz = nr1.displacement.dz.get/qb4D
        Console.println("kdz="+kdz)
        assertEquals(akdz,kdz,Vector.ACCURACY)
        val nr2:NodeResult = loadResult.maxMXXspan
        if(nr2!=null){
            val kmxx1 = nr2.stress.get.mxx.get/qb2
            Console.println("kmxx(H1)="+kmxx1)
            assertEquals(akmxx1,kmxx1,Vector.ACCURACY)
        }
        val nr3:NodeResult = loadResult.minMXXsupp
        if(nr3!=null){
            val kmxx2 = nr3.stress.get.mxx.get/qa2
            Console.println("kmxx(C)="+kmxx2)
            assertEquals(akmxx2,kmxx2,Vector.ACCURACY)
        }
        val nr5:NodeResult = loadResult.minMYYsupp
        if(nr5!=null){
            val kmyy1 = nr5.stress.get.myy.get/qa2
            Console.println("kmyy(H2)="+kmyy1)
            assertEquals(akmyy1,kmyy1,Vector.ACCURACY)
        }
        loadResult
    }

}
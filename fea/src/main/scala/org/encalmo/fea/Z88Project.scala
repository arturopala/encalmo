package org.encalmo.fea

import java.io.File
import scalax.io.{Output,Resource,Seekable}
import scalax.file.{Path}
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.io.InputStreamReader

/**
 * Z88(R) software project (www.z88.de)
 */
case class Z88Project[A <: FiniteElement](elemtype:FiniteElementType, loadCase:LoadCase[A], directory:Path) {
    
    val mesh = loadCase.mesh
    
    val SPACE = " "
    val CRLF = "\r\n"
    val LEGEND = "Written by ENCALMO Legend: C - at the corner, E - on the edge, S - on the surface, I - inside solid"

    def createInput:Unit = {
        Console.println("Creating Z88 project at "+directory.toURL)
        createInputFile_Z88I1
        createInputFile_Z88I2
        createInputFile_Z88I3(0)
        Seq("z88.dyn","z88o.ogl").foreach { f =>
	        val is = classOf[Z88Project[A]].getResourceAsStream("/"+f)
	        Resource.fromInputStream(is).copyDataTo(directory / f)
        }
}
    
    /** In Z88I1.TXT the geometry and material data of the structure are deposited */
    def createInputFile_Z88I1:Unit = {
        // GENERAL STRUCTURE DATA Z88I1.TXT
        val Z88I1 = directory / "z88i1.txt"
        Z88I1.createFile(true,false)
        Z88I1.write("") // clear previous content
        // 1st input group: General data in the first line, contains general structure data
        writeLine (Z88I1, mesh.dimension, mesh.nodes.size, mesh.elements.size, mesh.dof, loadCase.matlines, mesh.KFLAG, mesh.IBFLAG, mesh.IPFLAG, mesh.IQFLAG,LEGEND)
        // 2nd input group: Starting with line 2, contains coordinates of nodes, 
        // one line per node, node numbers strictly ascending.
        mesh.nodes foreach ( n =>
            writeLine (Z88I1, n.no, mesh.attr.dof ,n.x, n.y, n.z, n.positionSymbol)
        )
        // 3rd input group: Starting after last node, contains coincidence, i.e. the allocation of the element type and the 
        // corresponding nodes of every element. Edit two lines for every finite element. The element 
        // numbers, like the node numbers, must be entered strictly ascending.
        mesh.elements.sorted[FiniteElement] foreach ( e => {
                writeLine (Z88I1, e.no, e.attr.elemtype)
                write (Z88I1, e.nodes.map(_.no):_*)
                writeLine (Z88I1, " ",e.nodesPositionSymbol)
            }
        )
        //4th input group: Starting after last element contains Material information, one line for each material information. 
        loadCase.matgroups foreach (s => {
            write(Z88I1,s._3.head.no,s._3.last.no," ")
            writeLine(Z88I1,s._2:_*)
        })
        Console.println("Geometry and material data file (z88i1.txt) created.")
    }
    
    /** In Z88I2.TXT the boundary conditions and nodal forces are deposited */
    def createInputFile_Z88I2:Unit = {
        // BOUNDARY CONDITIONS Z88I2.TXT
        val Z88I2 = directory / "z88i2.txt"
        Z88I2.createFile(true,false)
        Z88I2.write("") // clear previous content
        // 2nd input group: Boundary conditions and loads. 
        // For every boundary condition and for every load respectively one line.
        val bclines = Iterator.from(0)
        loadCase.nodesConditions foreach ( nc => {
            // boundary conditions
            val dit = Iterator.from(1)
            nc.displacement.map(disp => convertToInputDisplacement(disp).foreach (di => di match {
                case Some(d) => { writeLine (Z88I2, nc.node.no, dit.next, 2, d, nc.node.positionSymbol); bclines.next }
                case None => dit.next
            }))
            // nodal forces
            val fit = Iterator.from(1)
            nc.force.map( force => force.seq.foreach (fi => fi match {
                case Some(f) => { writeLine (Z88I2, nc.node.no, fit.next, 1, f, nc.node.positionSymbol); bclines.next }
                case None => fit.next
            }))
        })
        val z88i2 = Z88I2.toString // take 2nd input back
        Z88I2.write("") // clear previous content
        // 1st input group: Number of the boundary conditions: loads and constraints
        writeLine (Z88I2, bclines.next, LEGEND)
        Z88I2.append(z88i2)
        Console.println("Boundary conditions file (z88i2.txt) created.")
    }
    
    /** In Z88I3.TXT stress calculation parameters are deposited
     *  Modes: 0 - corner nodes, 1 - gauss points */
    def createInputFile_Z88I3(mode:Int):Unit = {
        // STRESS PARAMETER FILE Z88I3.TXT
        val Z88I3 = directory / "z88i3.txt"
        Z88I3.createFile(true,false)
        Z88I3.write("") // clear previous content
        // File only consists of only one line: 
        // 1st value: Value of the integration order INTORD, 
        // 2nd value: For the plane stress elements No.3, 7, 11 and 14: KFLAG [Long] 
        // 0 = standard stress calculation, 
        // 1 = additional calculation of the radial and tangential stresses
        // 3rd value: Choice of the reduced stress hypothesis: ISFLAG 
        // 0 = no calculation of the reduced stresses
        // 1 = von Mises stresses
        // 2 = principal or Rankine stresses
        // 3 = Tresca stresses
        mode match {
            case 0 => writeLine (Z88I3, 0, 1, 0)
            case _ => writeLine (Z88I3, elemtype.intorder, 1, 1)
        }
        Console.println("Stress calculation parameters file (z88i3.txt) created.")
    }
    
    /** Runs displacements, stresses and nodal forces calculations: z88f-c, z88d and z88e */
    def runCalculations(debug:Boolean = false) = {
        import scala.collection.JavaConversions._
        val dir = new File(directory.toURL.getFile)
        // displacements calculation
        execute("z88f -c",dir)
        Console.println("Displacements calculated.")
        // stress calculation in the corners
        execute("z88d",dir)
        Console.println("Stresses in the corners calculated.")
        (directory / "z88o3.txt").moveTo(directory / "z88o3_c.txt",true)
        createInputFile_Z88I3(1)
        // stress calculation at gauss points
        execute("z88d",dir)
        Console.println("Stresses at the gauss points calculated.")
        // stress calculation
        execute("z88e",dir)
        Console.println("Nodal forces calculated.")
    }
    
    /** Executes system command */
    private def execute(command:String,dir:File) = {
        Console.println("Running "+command+" ...")
        val p = Runtime.getRuntime().exec(command,Array[String](), dir)
        var r = new InputStreamReader(p.getInputStream)
        var ch:Int = 0
        var sb = new StringBuilder
        while({ch = r.read; ch} != -1){
           sb.append(ch.asInstanceOf[Char])
        }
        val o = p.waitFor
        if(o!=0){
            Console.println(sb.toString)
            throw new IllegalStateException("Error executing system command: "+command)
        }
    }
    
    /** Reads and converts calculation's results */
    def readOutput:LoadResults[A] = {
        val displacements = readOutputFile_Z88O2.toMap
        val cornerStresses = readOutputFile_Z88O3(0).toMap
        val nodeStressesMap = mesh.elements.map(e => (e,cornerStresses(e.no)))
        	.flatMap(p => p._2.map(seq => {
	            p._1.findNode(extractCoordinatesFromStressOutput(seq)) match {
	              case Some(node) => (node.no,convertFromOutputCornerStress(seq))
	              case None => (-1,null)
	            }
		     }))
		     .groupBy(_._1)
		     .mapValues[NodeStress](seq => seq.size match {
		       	case 0 => NodeStress()
		       	case 1 => seq(0)._2
		       	case _ => NodeStress.aggregate(seq.map(_._2))
		     })
		Console.println("Nodes stresses map prepared.")
	    val gaussPointsStresses = readOutputFile_Z88O3(1).toMap
        val forces = readOutputFile_Z88O4.toMap
        val nodeResultsMap:Map[Int,NodeResult] = mesh.nodes.map(
            node => (node.no, NodeResult(node,
                    loadCase.conditionsForNode(node.no),
                    convertFromOutputDisplacement(displacements.get(node.no)),
                    nodeStressesMap.get(node.no),
                    convertFromOutputForce(forces(node.no))))
        ).toMap
        Console.println("Nodes results prepared.")
        val elementsResultsMap:Map[Int,ElementResult[A]]  = mesh.elements.map(
            element => (element.no, ElementResult(element,
                    element.nodesnumbers.map(nodeResultsMap(_)),
                    gaussPointsStresses(element.no).map(convertFromOutputGaussPointStress(_))))
        ).toMap
        Console.println("Elements results prepared.")
        LoadResults[A](loadCase,nodeResultsMap,elementsResultsMap)
    }
    
    /** Reads calculated displacements from Z88O2.TXT */
    def readOutputFile_Z88O2:Seq[(Int, Seq[Option[Double]])] = {
        val Z88O2 = directory / "z88o2.txt"
        if(Z88O2.exists){
            val lines = Z88O2.lines().dropWhile(s => !(s.trim.startsWith("1")))
            val displacements = lines.map(line => {
	                val l = line.trim.replaceAll("\\s+"," ").split(" ")
	                (l.head.toInt,l.tail.map(x => Option(x.toDouble)).toSeq)
            }).toSeq
            Console.println("Node's displacements read ("+displacements.size+").")
            displacements
        } else {
            Console.println("Displacements output file "+Z88O2.toURL.toExternalForm+" not found!")
            Seq()
        }
    }
    
    /** Reads calculated stresses from Z88O3.TXT. 
     *  Modes: 0 - corner nodes, 1 - gauss points */
    def readOutputFile_Z88O3(mode:Int) = {
        val Z88O3 = directory / ("z88o3"+(mode match {case 0 => "_c"; case _ => ""})+".txt")
        if(Z88O3.exists){
            val stresses = Z88O3.lines().dropWhile(s => !(s.trim.startsWith("element # = ")))
                .grouped((mode match {case 0 => elemtype.corners; case _ => elemtype.gausspoints}) + 2)
                .map(seq => {
                    val elemno = seq.head.drop(12).split(" ")(0).toInt
                    val stresses = seq.drop(2).take(elemtype.corners).map(line => {
                        val l = line.trim.replaceAll("\\s+"," ").split(" ")
                        l.map(x => Option(x.toDouble)).toSeq
                    })
                    (elemno,stresses)
            }).toSeq
            Console.println("Element's stresses "+(mode match {case 0 => "in the corners"; case _ => "at the gauss points"})+" read ("+stresses.size+").")
            stresses
        } else {
            Console.println("Stresses output file "+Z88O3.toURL.toExternalForm+" not found!")
            Seq()
        }
    }
    
    /** Reads calculated nodal forces from Z88O4.TXT */
    def readOutputFile_Z88O4:Seq[(Int, Seq[Option[Double]])] = {
        val Z88O4 = directory / "z88o4.txt"
        if(Z88O4.exists){
            val lines = Z88O4.lines().dropWhile(s => !(s.trim.startsWith("now the nodal sums for each node")))
            val forces = lines.drop(3).map(line => {
                    val l = line.trim.replaceAll("\\s+"," ").split(" ")
                    (l.head.toInt,l.tail.take(3).map(x => Option(x.toDouble)).toSeq)
            }).toSeq
            Console.println("Nodal forces read ("+forces.size+").")
            forces
        } else {
            Console.println("Nodal forces output file "+Z88O4.toURL.toExternalForm+" not found!")
            Seq()
        }
    }
    
    private def writeLine(out:Seekable,data:Any*) = {
        if(!data.isEmpty){
	        write(out,data:_*)
	        out.append(CRLF)
        }
    }
    
    private def write(out:Seekable,data:Any*) = {
        if(!data.isEmpty){
            append(out,data.head)
            if(!data.tail.isEmpty){
                data.tail.foreach(d => {
                    out.append(SPACE)
                    append(out,d)
                })
            }
        }
    }
    
    private def append(out:Seekable,data:Any):Unit = {
        data match {
            case None => out.append(0)
            case Some(x) => append(out,x)
            case i:Integer => leftPad(out,6,i.toString)
            case l:Long => leftPad(out,6,l.toString)
            case d:Double => leftPad(out,12,DoubleFormat.short(d))
            case _ => out.append(data.toString)
        }
    }
    
    private def leftPad(out:Seekable,size:Int,text:String):Unit = {
        (size - text.length) match {
            case x if x>0 => out.append(" " * x)
            case _ => Unit
        }
        out.append(text)
    }
    
    /** Converts output displacement to node displacement */
    def convertFromOutputDisplacement(disp:OptDoubleSeq):NodeDisplacement = {
        elemtype match {
            case Plate19Type => disp match {
                case None => NodeDisplacement()
                case Some(seq) => nodeDisplacementForPlate(seq)
            }
            case Plate20Type => disp match {
                case None => NodeDisplacement()
                case Some(seq) => nodeDisplacementForPlate(seq)
            }
            case _ => throw new IllegalStateException("Not implemented")
        }
    }
    /** Converts node displacement to input displacement */
    def convertToInputDisplacement(nd:NodeDisplacement):Seq[Option[Double]] = {
        elemtype match {
            case Plate19Type => Seq(nd.dz,nd.rx,nd.ry)
            case Plate20Type => Seq(nd.dz,nd.rx,nd.ry)
            case _ => throw new IllegalStateException("Not implemented")
        }
    }
    
    /** Converts output stress sequence to node coordinates */
    def extractCoordinatesFromStressOutput(seq:Seq[Option[Double]]):Vector = elemtype match {
  		case Plate19Type => Vector(seq(0).get,seq(1).get,0)
        case Plate20Type => Vector(seq(0).get,seq(1).get,0)
        case _ => throw new IllegalStateException("Not implemented")
    }
    
    /** Converts output stress sequence to NodeStress */
    def convertFromOutputCornerStress(seq:Seq[Option[Double]]):NodeStress = elemtype match {
  		case Plate19Type => nodeStressForPlateElementCorner(seq.drop(2))
        case Plate20Type => nodeStressForPlateElementCorner(seq.drop(2))
        case _ => throw new IllegalStateException("Not implemented")
    }
    
    def convertFromOutputGaussPointStress(seq:Seq[Option[Double]]):StressAtPoint = elemtype match {
        case Plate19Type => stressAtPointForPlateElementGaussPoint(seq)
        case Plate20Type => stressAtPointForPlateElementGaussPoint(seq)
        case _ => throw new IllegalStateException("Not implemented")
    }
    
    /** Converts output stress sequence to NodeStress */
    def convertFromOutputForce(seq:Seq[Option[Double]]):NodeForce = elemtype match {
        case Plate19Type => NodeForce(seq(1),seq(2),seq(0))
        case Plate20Type => NodeForce(seq(1),seq(2),seq(0))
        case _ => throw new IllegalStateException("Not implemented")
    }
    
    /** Plate corner stresses: MXX MYY MXY QYZ QZX SIGXX SIGYY TAUXY TAUXZ(Z=0) TAUYZ(Z=0) */
    def nodeStressForPlateElementCorner(seq:Seq[Option[Double]]):NodeStress = NodeStress(seq(5),seq(6),None,seq(7),seq(8),seq(9),None,seq(0),seq(1),seq(2),seq(3),seq(4))
    
    /** Plate gauss points stresses: MXX MYY MXY QYZ QZX SIGXX SIGYY TAUXY TAUXZ(Z=0) TAUYZ(Z=0) SIGV */
    def stressAtPointForPlateElementGaussPoint(seq:Seq[Option[Double]]):StressAtPoint = StressAtPoint(Vector(seq(0).get,seq(1).get,0),NodeStress(seq(7),seq(8),None,seq(9),seq(10),seq(11),seq(12),seq(2),seq(3),seq(4),seq(5),seq(6)))
    
    /** Plate node displacement: dz,rx,ry */
    def nodeDisplacementForPlate(dz:Double,rx:Double,ry:Double) = NodeDisplacement(None,None,Some(dz),Some(rx),Some(ry),None)
    def nodeDisplacementForPlate(dz:Option[Double] = None,rx:Option[Double] = None, ry:Option[Double] = None) = NodeDisplacement(None,None,dz,rx,ry,None)
    def nodeDisplacementForPlate(seq:Seq[Option[Double]]) = NodeDisplacement(None,None,seq(0),seq(1),seq(2),None)
}
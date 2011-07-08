package org.encalmo.fea

import java.io.File
import scalax.io.{Output,Resource,Seekable}
import scalax.file.{Path}
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

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
        val is = classOf[Z88Project[A]].getResourceAsStream("/z88.dyn")
        Resource.fromInputStream(is).copyData(directory / "z88.dyn")
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
            nc.forces.map( force => force.seq.foreach (fi => fi match {
                case Some(f) => { writeLine (Z88I2, nc.node.no, fit.next, 1, f, nc.node.positionSymbol); bclines.next }
                case None => fit.next
            }))
        })
        val z88i2 = Z88I2.slurpString // take 2nd input back
        Z88I2.write("") // clear previous content
        // 1st input group: Number of the boundary conditions: loads and constraints
        writeLine (Z88I2, bclines.next, LEGEND)
        Z88I2.append(z88i2)
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
    }
    
    /** Runs displacements, stresses and nodal forces calculations: z88f-c, z88d and z88e */
    def runCalculations(debug:Boolean = false) = {
        import scala.collection.JavaConversions._
        val dir = new File(directory.toURL.getFile)
        // displacements calculation
        val p1 = Runtime.getRuntime().exec("z88f -c",Array[String](), dir)
        p1.waitFor
        if(debug) {
            val p1log = Resource.fromInputStream(p1.getInputStream).slurpString
            Console.println(p1log)
        }
        Console.println("Displacements calculated.")
        // stress calculation in the corners
        val p2c = Runtime.getRuntime().exec("z88d",Array[String](), dir)
        p2c.waitFor
        if(debug) {
            val p2clog = Resource.fromInputStream(p2c.getInputStream).slurpString
            Console.println(p2clog)
        }
        Console.println("Stresses in the corners calculated.")
        (directory / "z88o3.txt").moveTo(directory / "z88o3_c.txt",true)
        createInputFile_Z88I3(1)
        // stress calculation at gauss points
        val p2g = Runtime.getRuntime().exec("z88d",Array[String](), dir)
        p2g.waitFor
        if(debug) {
            val p2glog = Resource.fromInputStream(p2g.getInputStream).slurpString
            Console.println(p2glog)
        }
        Console.println("Stresses at the gauss points calculated.")
        // stress calculation
        val p3 = Runtime.getRuntime().exec("z88e",Array[String](), dir)
        p3.waitFor
        if(debug) {
            val p3log = Resource.fromInputStream(p3.getInputStream).slurpString
            Console.println(p3log)
        }
        Console.println("Nodal forces calculated.")
    }
    
    /** Reads all calculation's results */
    def readOutput:LoadResults[A] = {
        val displacements = readOutputFile_Z88O2.toMap
        val cornerStresses = readOutputFile_Z88O3(0).toMap
        /*mesh.elements.map(e => (e,cornerStresses(e.no))).map(p => p._2.map(seq => {
            val node = p._1.findNode(seq(0),seq(1))
        }))*/
        val gaussPointsStresses = readOutputFile_Z88O3(1).toMap
        val forces = readOutputFile_Z88O4.toMap
        val nodeResult = mesh.nodes.map(
            node => NodeResult(node,
                    loadCase.conditionsForNode(node.no),
                    convertFromOutputDisplacement(displacements.get(node.no)),
                    NodeForce(forces.get(node.no)))
        )
        LoadResults[A](loadCase,nodeResult)
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
                case Some(seq) => NodeDisplacement.forPlate(seq)
            }
            case Plate20Type => disp match {
                case None => NodeDisplacement()
                case Some(seq) => NodeDisplacement.forPlate(seq)
            }
            case _ => null
        }
    }
    /** Converts node displacement to input displacement */
    def convertToInputDisplacement(nd:NodeDisplacement):Seq[Option[Double]] = {
        elemtype match {
            case Plate19Type => Seq(nd.dz,nd.rx,nd.ry)
            case Plate20Type => Seq(nd.dz,nd.rx,nd.ry)
            case _ => null
        }
    }
    
}
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
case class Z88Project[A <: FiniteElement](loadCase:LoadCase[A], directory:Path) {
    
    val mesh = loadCase.mesh
    
    val SPACE = " "
    val CRLF = "\r\n"
    val LEGEND = "Written by ENCALMO Legend: C - at the corner, E - on the edge, S - on the surface, I - inside solid"

    def createInputFiles:Unit = {
        Console.println("Creating Z88 project at "+directory.toString)
        createInputFile_Z88I1
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
        // BOUNDARY CONDITIONS Z88I2.TXT
        val Z88I2 = directory / "z88i2.txt"
        Z88I2.createFile(true,false)
        Z88I2.write("") // clear previous content
        // 2nd input group: Boundary conditions and loads. 
        // For every boundary condition and for every load respectively one line.
        val bclines = Iterator.from(0)
        loadCase.nodeCases foreach ( nc => {
            // boundary conditions
            val dit = Iterator.from(1)
            nc.displacement.map(_.foreach (di => di match {
                case Some(d) => { writeLine (Z88I2, nc.node.no, dit.next, 2, d, nc.node.positionSymbol); bclines.next }
                case None => dit.next
            }))
            // nodal forces
            val fit = Iterator.from(1)
            nc.force.map(_.foreach (fi => fi match {
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
            case d:Double => leftPad(out,12,Format.ShortDouble.format(d))
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
    
}

object Format {
    
    lazy val ShortDouble = new DecimalFormat("0.000000", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
    
}
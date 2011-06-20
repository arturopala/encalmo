package org.encalmo.fea.z88

import java.io.File
import scalax.io.{Output,Resource,Seekable}
import scalax.file.{Path}
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

/**
 * Z88(R) software project (www.z88.de)
 */
case class Z88Project[A <: FiniteElement](mesh:Mesh[A], directory:Path) {
    
    val SPACE = " "
    val CRLF = "\r\n"

    def createInputFiles:Unit = {
        Console.println("Creating Z88 project at "+directory.toURL)
        createInputFile_Z88I1
    }
    
    /** In Z88I1.TXT the geometry and material data of the structure are deposited */
    def createInputFile_Z88I1:Unit = {
        // GENERAL STRUCTURE DATA Z88I1.TXT
        val Z88I1 = directory / "z88i1.txt"
        Z88I1.createFile(true,false)
        Z88I1.write("")
        // 1st input group: General data in the first line, contains general structure data
        writeLine (Z88I1, mesh.dimension, mesh.nodes.size, mesh.elements.size, mesh.dof, mesh.matlines, mesh.KFLAG, mesh.IBFLAG, mesh.IPFLAG, mesh.IQFLAG,"Written by ENCALMO Legend: C - at the corner, E - on the edge, S - on the surface, I - inside solid")
        // 2nd input group: Starting with line 2, contains coordinates of nodes, 
        // one line per node, node numbers strictly ascending.
        mesh.nodes foreach ( n =>
            writeLine (Z88I1, n.no, mesh.e.attr.dof ,n.x, n.y, n.z, n.positionSymbol)
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
        mesh.matgroups foreach (s => {
            write(Z88I1,s._3.head.no,s._3.last.no," ")
            writeLine(Z88I1,s._2:_*)
        })

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
            case d:Double => leftPad(out,12,Format.ShortDouble.format(d))
            case i:Integer => leftPad(out,6,i.toString)
            case l:Long => leftPad(out,6,l.toString)
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
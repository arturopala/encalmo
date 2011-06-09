package org.encalmo.fea.z88

import java.io.File
import scalax.io.{Output,Resource,Seekable}
import scalax.file.{Path}

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
        val Z88I1 = directory / "Z88I1.TXT"
        Z88I1.createFile(true,false)
        Z88I1.write("")
        // 1st input group: General data in the first line, contains general structure data
        writeLine (Z88I1, mesh.dimension, mesh.size, mesh.dof, mesh.matlines, mesh.KFLAG, mesh.IBFLAG, mesh.IPFLAG, mesh.IQFLAG)
        // 2nd input group: Starting with line 2, contains coordinates of nodes, 
        // one line per node, node numbers strictly ascending.
        

    }
    
    private def writeLine(out:Seekable,data:Any*) = {
        if(!data.isEmpty){
	        out.append(data.head.toString)
	        if(!data.tail.isEmpty){
	        	data.tail.foreach(d => {
	        	    out.append(SPACE)
	        	    out.append(d.toString)
	        	})
	        }
	        out.append(CRLF)
        }
    }
    
}
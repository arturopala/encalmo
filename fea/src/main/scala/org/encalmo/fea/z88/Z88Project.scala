package org.encalmo.fea.z88

import java.io.File
import scalax.io.{Output,Resource}
import scalax.file.{Path}

/**
 * Z88(R) software project (www.z88.de)
 */
case class Z88Project[A <: FiniteElement](mesh:Mesh[A], directory:Path) {
    
    val SPACE = ""
    val CRLF = "\r\n"

    def createInputFiles:Unit = {
        createInputFile_Z88I1
    }
    
    /** In Z88I1.TXT the geometry and material data of the structure are deposited */
    def createInputFile_Z88I1:Unit = {
        val Z88I1 = directory / "Z88I1.TXT"
        //1st input group: General data in the first line, contains general structure data
        writeLine (Z88I1, mesh.dimension, mesh.size, mesh.dof, mesh.matlines, mesh.KFLAG, mesh.IBFLAG, mesh.IPFLAG, mesh.IQFLAG)
    }
    
    private def writeLine(out:Output,data:Any*) = {
        if(!data.isEmpty){
	        out.write(data.head.toString)
	        if(!data.tail.isEmpty){
	        	data.tail.foreach(d => {
	        	    out.write(SPACE)
	        	    out.write(d.toString)
	        	})
	        }
	        out.write(CRLF)
        }
    }
    
}
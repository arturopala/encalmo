package org.encalmo.examples

import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps
import org.junit.Test
import au.com.bytecode.opencsv._
import java.io.FileReader


/**
 * DatasheetImporter
 * @author artur.opala
 */
class DatasheetImporter {

	@Test
	def importIPE = doImport("IPE","ipe")
	
	def readCSV(file:String, fx:(String,String) => Unit) {
		 val reader:CSVReader = new CSVReader(new FileReader(file));
		 val rows:Seq[Array[String]] = reader.readAll();
		 for(row <- rows.tail){
			 val b = new StringBuilder
			 b.append("\"")
			 b.append(row.head)
			 b.append("\"")
			 for(cell <- row.tail) {
				 b.append(",")
				 if(cell.isEmpty) {
					 b.append("0")
				 }else{
					b.append(cell)
				 }
			 }
			 fx(row.head,b.toString)
		 }

	}
	
	def doImport(clazz:String, file:String) {
		val b1 = new StringBuilder
		val b2 = new StringBuilder
		b2.append("\r\n\tval map = Map[String,()=>"+clazz+"](\r\n\t\t")
		var counter:Int = 0
		readCSV("examples/src/main/resources/datasheet/"+file+".csv", (id,params) => {
			val name = fieldName(id.trim)
			//object
			b1.append("\tlazy val ")
			b1.append(name)
			b1.append(" = ")
			b1.append(clazz)
			b1.append("(")
			b1.append(params)
			b1.append(")\r\n")
			//map entry
			if(counter>0) b2.append(",\r\n\t\t")
			b2.append("\"")
			b2.append(id)
			b2.append("\"")
			b2.append(" -> ")
			b2.append(name)
			b2.append(" _")
			counter = counter + 1
		})
		b2.append("\r\n\t)\r\n")
		Console.println("\t/*---------------------------------------")
		Console.print("\t *\t")
		Console.println(clazz)
		Console.println("\t *---------------------------------------*/")
		Console.println(b2)
		Console.println(b1)
	}
	
	private def fieldName(id:String):String = new StringOps(id).map(x => x match {
        case ' ' => '_'
        case ',' => '_'
        case ':' => '_'
        case _ => x
    })

}
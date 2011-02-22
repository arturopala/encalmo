package org.encalmo.examples

import scala.collection.JavaConversions._
import org.junit.Test
import au.com.bytecode.opencsv._
import java.io.FileReader


/**
 * DatasheetImporter
 * @author artur.opala
 */
class DatasheetImporter {
	
	
	def doImport(file:String, fx:(String) => Unit) {
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
			 fx(b.toString)
		 }

	}
	
	@Test
	def test {
		doImport("examples/src/main/resources/datasheet/ipe.csv", s => {
			Console.println(s)
		})
	}

}
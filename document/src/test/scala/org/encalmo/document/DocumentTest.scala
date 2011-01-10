package org.encalmo.document

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

/**
 * Document test
 * @author artur
 */
class DocumentTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def testDocument1() {
	    
	    val doc1 = new Document("Test",
	            EmptyDocumentComponent,
	            EmptyDocumentComponent,
	            EmptyDocumentComponent)
	    
	}
	
}
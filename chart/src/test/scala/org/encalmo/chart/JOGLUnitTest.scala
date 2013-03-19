package org.encalmo.chart

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/** JOGL initialization test */
class JOGLUnitTest {
    
    @Test def testJOGLInit:Unit = {
        try {
        	JOGL
        }
        catch {
            case e => fail(e.getMessage)
        }
    }
	
}
package org.encalmo.chart

import org.junit.{Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/** JOGL initialization test */
@Ignore
class JOGLUnitTest {
    
    @Test def testJOGLInit:Unit = {
        try {
        	JOGL
        }
        catch {
            case e: Throwable => fail(e.getMessage)
        }
    }
	
}
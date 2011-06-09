package org.encalmo.fea.z88

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 * 
 * @author artur.opala
 */
class NodeTest {
    
    @Test def test1 = {
        val n1 = Node(0d,0d,0d)
        val n2 = Node(1d,1d,1d)
        val n3 = Node(1d,1d,5d)
        val n4 = Node(5d,1d,1d)
        assertTrue((n1 compare n2) < 0)
        assertTrue((n1 compare n3) < 0)
        assertTrue((n1 compare n4) < 0)
        assertTrue((n2 compare n1) > 0)
        assertTrue((n3 compare n1) > 0)
        assertTrue((n4 compare n1) > 0)
        assertTrue((n2 compare n2) == 0)
        assertTrue((n3 compare n3) == 0)
        assertTrue((n4 compare n4) == 0)
        assertTrue((n2 compare n3) < 0)
        assertTrue((n2 compare n4) < 0)
        assertTrue((n2 compare n1) > 0)
        assertTrue((n3 compare n4) > 0)
        assertTrue((n4 compare n3) < 0)
    }
    
}
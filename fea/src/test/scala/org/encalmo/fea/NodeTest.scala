package org.encalmo.fea

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 * Node unit tests
 * @author artur.opala
 */
class NodeTest {
    
    def assertAscending(f:(Boolean)=>Unit)(n:Node*):Unit = {
        f(n.dropRight(1).zip(n.tail).forall(x => {(x._1 compare x._2) < 0 && (x._2 compare x._1) > 0}))
    }
    
    def assertAscendingTrue(n:Node*) = assertAscending(assertTrue _)_
    def assertAscendingFalse(n:Node*) = assertAscending(assertFalse _)_
    
    @Test def test1:Unit = {
        val n1 = Node(0d,0d,0d)
        val n2 = Node(1d,1d,1d)
        val n3 = Node(1d,1d,5d)
        val n4 = Node(5d,1d,1d)
        assertAscendingTrue(n1,n2,n3,n4)
        assertAscendingFalse(n4,n1,n2,n3)
        // test after renumbering
        Mesh.renumber(Seq(n1,n2,n3,n4))
        assertAscendingTrue(n1,n2,n3,n4) 
        assertAscendingFalse(n4,n1,n2,n3) 
    }
    
    @Test def test2:Unit = {
        val n1 = Node(0d,0d,0d)
        val n2 = Node(1d,1d,1d)
        val n3 = Node(1d,1d,5d)
        val n4 = Node(5d,1d,1d)
        assertAscendingTrue(n1,n2,n3,n4)
        assertAscendingFalse(n4,n1,n2,n3)
        // test after renumbering
        Mesh.renumber(Seq(n4,n1,n2,n3))
        assertAscendingTrue(n4,n1,n2,n3) 
        assertAscendingFalse(n1,n2,n3,n4) 
    }
    
}
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
        val grid  = Grid.fromDiagonal(Vector(1d,1d,1d))
        val n1 = Node(grid,0,0,0)
        val n2 = Node(grid,1,1,1)
        val n3 = Node(grid,1,1,5)
        val n4 = Node(grid,5,1,1)
        assertAscendingTrue(n1,n2,n3,n4)
        assertAscendingFalse(n4,n1,n2,n3)
        // test after renumbering
        Mesh.renumber(Seq(n1,n2,n3,n4))
        assertAscendingTrue(n1,n2,n3,n4) 
        assertAscendingFalse(n4,n1,n2,n3) 
    }
    
    @Test def test2:Unit = {
        val grid  = Grid.fromDiagonal(Vector(1d,1d,1d))
        val n1 = Node(grid,0,0,0)
        val n2 = Node(grid,1,1,1)
        val n3 = Node(grid,1,1,5)
        val n4 = Node(grid,5,1,1)
        assertAscendingTrue(n1,n2,n3,n4)
        assertAscendingFalse(n4,n1,n2,n3)
        // test after renumbering
        Mesh.renumber(Seq(n4,n1,n2,n3))
        assertAscendingTrue(n4,n1,n2,n3) 
        assertAscendingFalse(n1,n2,n3,n4) 
    }
    
}
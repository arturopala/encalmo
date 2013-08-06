package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.graph.Graph

class SymbolsGraphUnitTest extends AssertionsForJUnit {
	
	@Test def shouldBuildGraph = {
        //given
		import BasicSymbols._
		implicit val calc = Calculation()
		a := b * (c - d)
        b := c + 10
        c := e - f
        d := h * (e/10)
        e := z - 0.3
        //when
        val graph = SymbolsGraph.build(calc)
        //then
        for(symbol <- calc.listSymbols) {
            assertTrue(graph.contains(symbol));
        }
        assertFalse(Graph.hasCycles(graph));
        val list = Graph.sortTopologically(graph)
        assertTrue(list.sameElements(List(z,f,e,h,d,c,b,a)))
	}
}
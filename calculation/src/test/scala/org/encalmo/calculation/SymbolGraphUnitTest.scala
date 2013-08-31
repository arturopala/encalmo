package org.encalmo.calculation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import org.encalmo.graph.Graph

class SymbolGraphUnitTest extends AssertionsForJUnit {

  @Test def shouldBuildGraphFromSingleCalculation() = {
    //given
    import BasicSymbols._
    implicit val calc = Calculation()
    a := b * (c - d)
    b := c + 10
    c := hypot(-e, abs(f))
    d := h * (e / 10)
    e := sin(z) - 0.3
    //when
    val graph = SymbolGraph.build(calc)
    //then
    for (symbol <- calc.listSymbols) {
      assertTrue(graph.contains(symbol))
    }
    assertFalse(Graph.hasCycles(graph))
    val list = Graph.sortTopologically(graph)
    assertTrue(list.sameElements(List(z, h, f, e, d, c, b, a)))
  }

  @Test def shouldBuildGraphFromCompositeCalculation() = {
    //given
    import BasicSymbols._
    val calc = Calculation()
    val calc1 = Calculation()
    val calc2 = Calculation()
    calc(a) = b * (c - d)
    calc(b) = c + 10
    calc1(c) = e - f
    calc1(d) = h * (e / 10)
    calc2(e) = z - 0.3
    calc add calc1
    calc1 add calc2
    //when
    val graph = SymbolGraph.build(calc)
    //then
    for (symbol <- calc.listSymbols) {
      assertTrue(graph.contains(symbol))
    }
    assertFalse(Graph.hasCycles(graph))
    val list = Graph.sortTopologically(graph)
    assertTrue(list.sameElements(List(z, h, f, e, d, c, b, a)))
  }

  @Test def shouldBuildGraphAndFindCycle() = {
    //given
    import BasicSymbols._
    implicit val calc = Calculation()
    a := b * (c - d)
    b := c + 10
    c := e - f
    d := h * (e / 10)
    e := z - a
    //when
    val graph = SymbolGraph.build(calc)
    //then
    for (symbol <- calc.listSymbols) {
      assertTrue(graph.contains(symbol))
    }
    assertTrue(Graph.hasCycles(graph))
    assertTrue(Graph.findCycles(graph).sameElements(Vector(a,a,a)))
  }

    @Test def shouldBuildGraphFromSingleCalculationWithDynamic() = {
        //given
        import BasicSymbols._
        implicit val calc = Calculation()
        a := b * (c - d)
        b := c + 10
        c := hypot(-e, abs(f))
        d := h * (e / 10)
        e := sin(z) - 0.3
        g := dynamic(a,b) { results =>
            results(a).toInt match {
                case 0 => b
                case _ => a*b
            }
        }
        //when
        val graph = SymbolGraph.build(calc)
        //then
        for (symbol <- calc.listSymbols) {
            assertTrue(graph.contains(symbol))
        }
        assertFalse(Graph.hasCycles(graph))
        val list = Graph.sortTopologically(graph)
        assertEquals(List(z,h, f, e, d, c, b, a, g), list)
    }

}
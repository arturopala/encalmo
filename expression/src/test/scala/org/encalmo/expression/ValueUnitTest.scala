package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._
import java.io.{StringWriter,PrintWriter}

class ValueUnitTest extends AssertionsForJUnit {
    
    @Test def test1 = {
        
        def sum(n1:Value,n2:Value) = (n1,n2) match {
            case (n1:Number,n2:Number) => Number(n1.r+n2.r)
            case _ => throw new UnsupportedOperationException
        }
        
        Value.register("+", classOf[Number], classOf[Number], sum _)
        
        val n1 = 3
        val n2 = 4
        
        val r = Value.execute("+", n1, n2)
        
        assertEquals(Number(7),r.get)
        
    }
    
    @Test def test2 = {
        
        def sin(v1:Value) = v1 match {
            case n1:Number => Number(n1.r.rad.sin)
            case _ => throw new UnsupportedOperationException
        }
        
        Value.register("sin", classOf[Number], sin _)
        
        val n1 = 30
        
        val r = Value.execute("sin", n1)
        
        assertEquals(Number(0.5),r.get)
        
    }

}
package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.io.{StringWriter,PrintWriter}

class OperationUnitTest extends AssertionsForJUnit {
	
	import BasicSymbols._
	
	@Test def verifySum1() {
		val e1:Expression = Number(3.8)+Number(6.2)
		assertEquals(Number(10),e1.eval)
		val e2:Expression = Number(3.8)+Number(6.2)+Number(12.1)
		assertEquals(Number(22.1),e2.eval)
		val e3:Expression = Number(3.8)+(Number(6.2)+Number(12.1))
		assertEquals(Number(22.1),e3.eval)
		val e4:Expression = e3+e2
		assertEquals(Number(44.2),e4.eval)
		//assertEquals(classOf[MultiSum],e3.getClass())
		//assertEquals(classOf[MultiSum],e4.getClass())
	}
	
	@Test def verifyProd1() {
		val e1:Expression = Number(3.8)*Number(6.2)
		assertEquals(Number(23.56),e1.eval)
		val e2:Expression = Number(3.8)*Number(6.2)*Number(12.1)
		assertEquals(Number(285.076),e2.eval)
		val e3:Expression = Number(3.8)*(Number(6.2)*Number(12.1))
		assertEquals(Number(285.076),e3.eval)
		val e4:Expression = e3*e2
		assertEquals(Number(81268.325776),e4.eval)
		dumpRaw(e1,e2,e3,e4)
		//assertEquals(classOf[MultiProd],e3.getClass())
		//assertEquals(classOf[MultiProd],e4.getClass())
	}
	
	@Test def verifyMin1() {
		import Symbol._
		val e1:Expression = min(3,4,5,6,7,8,9,10)
		assertEquals(e1.eval,Number(3))
	}
	
	@Test def verifyMin2() {
		import Symbol._
		val e2:Expression = 10/2d;
		val e3:Expression = 10/3d;
		val e4:Expression = 10/4d;
		val e1:Expression = min(e2,e3,e4)
		assertEquals(Number(2.5),e1.eval)
	}
	
	@Test def verifyMin3() {
		import Symbol._
		val e2:Expression = sin(10/2d);
		val e3:Expression = sin(10/3d);
		val e4:Expression = sin(10/4d);
		val e1:Expression = min(e2,e3,e4)
		val t1 = e1.eval
		val t2 = e4.eval
		assertEquals(t1,t2)
	}
	
	@Test def verifyMax1() {
		import Symbol._
		val e1:Expression = max(3,4,5,6,7,8,9,10)
		assertEquals(Number(10),e1.eval)
	}
	
	@Test def verifyMax2() {
		import Symbol._
		val e2:Expression = 10/2d;
		val e3:Expression = 10/3d;
		val e4:Expression = 10/4d;
		val t1:Expression = max(e2,e3,e4)
		val t2:Expression = min(e2,e3,e4)
		assertEquals(Number(5),t1.eval)
		assertEquals(e4,t2.eval)
	}
	
	@Test def verifyMax4() {
		import Symbol._
		val e2:Expression = sin(10/2d);
		val e3:Expression = sin(10/3d);
		val e4:Expression = sin(10/4d);
		val e1:Expression = min(e2,e3,e4)
		assertEquals(e1.eval,e4.eval)
	}
	
	@Test def verifyPi1() {
		import Symbol._
		val e1:Expression = PI*10
		assertEquals(Number(31.41592653589793),e1.eval)
	}
	
	@Test def verifyE1() {
		import Symbol._
		val e1:Expression = EUL*PI
		assertEquals(Number(8.539734222673566),e1.eval)
	}
	
	@Test def verifyMultiSum() {
		import BasicSymbols.a
		val e1:Expression = MultiSum(1,2,3,4,5,6,7)
		assertEquals(Number(28),e1.eval)
		val e2:Expression = MultiSum(1,2,3,4,5,6,a)
		assertEquals(MultiSum(21,a),e2.eval)
		val e3:Expression = MultiSum(1,1,1,1,1,1,1)
		assertEquals(Number(7),e3.eval)
		assertFalse(ONE==e3.eval)
		val e4:Expression = MultiSum(0,0,0,0,0,0,0)
		assertEquals(ZERO,e4.eval)
		assertFalse(Number(7)==e4.eval)
	}
	
	@Test def verifyMultiProd() {
		import BasicSymbols.a
		val e1:Expression = MultiProd(1,1,1,1,1,1,1)
		assertEquals(One,e1.eval)
		val e3:Expression = MultiProd(0,0,0,0,0,0,0)
		assertEquals(Zero,e3.eval)
		val e2:Expression = MultiProd(1,2,3,4,5,6,a)
		assertEquals(MultiProd(720,a),e2.eval)
		val e4:Expression = MultiProd(1,0,0,0,0,0,0)
		assertEquals(Zero,e4.eval)
		val e5:Expression = MultiProd(1,1,1,1,1,1,0)
		assertEquals(Zero,e5.eval)
	}
	
	@Test def verifyOperationN() {
		val testArgs = Seq(1,2,3,4,5,6,7)
		val oN:OperationN = min(1,2,3,4,5,6,7)
		val args = oN match {
			case OperationN(args,o) => args
		}
		assertTrue(testArgs.zip(args).forall(x => (x._1 == x._2.asInstanceOf[Number].r.d)))
		assertFalse(testArgs.zip(args).forall(x => (x._1 != x._2.asInstanceOf[Number].r.d)))
	}
	
	

}
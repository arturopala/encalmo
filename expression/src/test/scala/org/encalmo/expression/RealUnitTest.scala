package org.encalmo.expression

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.encalmo.expression._

class RealUnitTest extends AssertionsForJUnit {
	
	def isEqual(r1:Real,r2:Real) = assertEquals(r1, r2);
	def notEqual(r1:Real,r2:Real) = assertNotSame(r1, r2);
	
	@Test def verifyEquals() {
		isEqual(Real(1), Real(1));
		isEqual(Real(1), Real.one);
		isEqual(Real(0), Real(0));
		isEqual(Real(0), Real.zero);
		isEqual(Real(23456), Real(23456));
		isEqual(Real(0.1), Real(0.10));
		isEqual(Real(0.23456), Real(0.234560));
		isEqual(0.123, 0.1230);
	}
	
	@Test def verifyNotEquals() {
		assertNotSame(Real(1), Real(0));
		assertNotSame(Real(1), Real(2));
		assertNotSame(Real(0.23456), Real(0.234561));
		notEqual(0.123, 0.1231);
	}
	
	@Test def verifyAddition() {
		assertEquals(Real(1)+Real(0),Real(1));
		assertEquals(Real(1)+Real(2), Real(3));
		assertNotSame(Real(1)+Real(2), Real(4));
		assertEquals(Real(1.1)+Real(0.1),Real(1.2));
		assertEquals(Real(1.5)+Real(2.54), Real(4.040));
		assertNotSame(Real(1.1)+Real(2.1), Real(3.21));
	}
	
	@Test def verifySubtraction() {
		assertEquals(Real(1)-Real(0),Real(1));
		assertEquals(Real(1)-Real(2), Real(-1));
		assertNotSame(Real(1)-Real(2), Real(1));
		assertEquals(Real(1.1)-Real(0.1),Real(1));
		assertEquals(Real(1.5)-Real(2.54), Real(-1.040));
		assertNotSame(Real(1.1)-Real(2.1), Real(-1));
	}
	
	@Test def verifyMultiplication() {
		isEqual(Real(1)*Real(0),Real(0));
		assertEquals(Real(1)*Real(2), Real(2));
		assertNotSame(Real(1)*Real(2), Real(3));
		assertEquals(Real(1.1)*Real(0.1),Real(0.11));
		isEqual(Real(1.5)*Real(2.54), 3.81);
		assertNotSame(Real(1.1)*Real(-2.1), Real(2.3100));
	}
	
}
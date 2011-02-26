package org.encalmo.calculation

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit test for the org.encalmo.expression package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[ContextUnitTest],
	classOf[ContextSetUnitTest],
	classOf[CalculationUnitTest]
))
class TestSuite
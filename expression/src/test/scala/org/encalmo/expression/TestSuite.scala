package org.encalmo.expression

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit test for the org.encalmo.expression package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[RealUnitTest],
	classOf[NumberUnitTest],
	classOf[ExpressionUnitTest],
	classOf[SelectionUnitTest],
	classOf[OperationUnitTest]
))
class TestSuite
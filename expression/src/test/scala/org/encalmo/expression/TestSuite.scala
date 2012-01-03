package org.encalmo.expression

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit tests for the org.encalmo.expression package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[RealUnitTest],
	classOf[NumberUnitTest],
	classOf[ExpressionUnitTest],
	/*classOf[SelectionUnitTest],*/
	classOf[OperationUnitTest],
	classOf[TransformationsUnitTest],
	classOf[ValueUnitTest],
	classOf[UnitOfValueUnitTest],
    classOf[SISystemUnitTest],
	classOf[NumberUnitOfValueUnitTest],
	classOf[UnitsTransformationsUnitTest]
))
class TestSuite
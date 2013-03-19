package org.encalmo.printer.expression

import org.encalmo.printer._
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit test for the org.encalmo.printer.expression package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[PlainTextExpressionPrinterTest],
	classOf[MathMLExpressionPrinterUnitTest]
))
class TestSuite
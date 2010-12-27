package org.encalmo.printer

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit test for the org.encalmo.expression package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[PlainTextPrinterTest]
))
class TestSuite
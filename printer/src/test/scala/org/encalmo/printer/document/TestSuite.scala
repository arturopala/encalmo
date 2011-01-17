package org.encalmo.printer.document

import org.encalmo.printer._
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Suite of unit test for the org.encalmo.printer.document package
 * @author artur.opala
 */
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
	classOf[PlainTextDocumentPrinterTest]
))
class TestSuite
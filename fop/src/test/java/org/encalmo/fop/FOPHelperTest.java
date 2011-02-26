package org.encalmo.fop;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Properties;

import org.apache.fop.apps.FormattingResults;
import org.junit.Test;

public class FOPHelperTest {
    
    @Test
    public final void test1(){
        StringBuffer doc = FOPHelper.buildFO("mathml.fo", new Properties());
        try {
        	FormattingResults results = FOPHelper.buildPDF(doc, "target/test-results/mathml.pdf");
        	assertTrue(results.getPageCount()>0);
        } catch (Exception e) {
			fail(e.getMessage());
		} 
    }
    
    @Test
    public final void test2(){
        StringBuffer doc = FOPHelper.buildFO("test.fo", new Properties());
        try {
        	FormattingResults results = FOPHelper.buildPDF(doc, "target/test-results/test.pdf");
        	assertTrue(results.getPageCount()>0);
        } catch (Exception e) {
			fail(e.getMessage());
		} 
    }
    
}

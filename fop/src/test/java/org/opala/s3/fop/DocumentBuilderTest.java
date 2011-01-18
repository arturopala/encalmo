package org.opala.s3.fop;

import java.io.IOException;
import java.util.Properties;

import org.apache.fop.apps.FOPException;
import org.junit.Test;

public class DocumentBuilderTest{
    
    /*@Test
    public final void test1(){
        StringBuffer doc = DocumentBuilder.buildFO("mathml.fo", new Properties());
        try {
			DocumentBuilder.buildPDF(doc, "target/test-results/mathml.pdf");
		} catch (FOPException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
    }*/
    
    @Test
    public final void test2(){
        StringBuffer doc = DocumentBuilder.buildFO("test.fo", new Properties());
        try {
			DocumentBuilder.buildPDF(doc, "target/test-results/test.pdf");
		} catch (FOPException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
    }
    
}

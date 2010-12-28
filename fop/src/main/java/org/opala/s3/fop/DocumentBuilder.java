package org.opala.s3.fop;


import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamSource;

import org.apache.avalon.framework.configuration.DefaultConfigurationBuilder;
import org.apache.fop.apps.FOPException;
import org.apache.fop.apps.FOUserAgent;
import org.apache.fop.apps.Fop;
import org.apache.fop.apps.FopFactory;
import org.apache.fop.apps.FormattingResults;
import org.apache.fop.apps.MimeConstants;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import freemarker.template.Configuration;
import freemarker.template.DefaultObjectWrapper;
import freemarker.template.Template;
import freemarker.template.TemplateException;

public class DocumentBuilder {
    
    private static Configuration cfg;
    private static FopFactory fopFactory = FopFactory.newInstance();

    static{
        cfg = new Configuration();
        cfg.setClassForTemplateLoading(DocumentBuilder.class, "/ftl/");
        cfg.setObjectWrapper(new DefaultObjectWrapper());
        
        DefaultConfigurationBuilder fopcfgBuilder = new DefaultConfigurationBuilder();
        org.apache.avalon.framework.configuration.Configuration fopcfg;
        try {
            fopcfg = fopcfgBuilder.build(DocumentBuilder.class.getResourceAsStream("/fop.xconf"));
            fopFactory.setUserConfig(fopcfg);
            fopFactory.setBaseURL(DocumentBuilder.class.getResource("/").toExternalForm());
            fopFactory.setFontBaseURL(DocumentBuilder.class.getResource("/fonts/").toExternalForm());
        } 
        catch(RuntimeException re){
            throw re;
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        } 
    }
    
   public static StringBuffer buildFO(String template, Object model){
       Template temp;
        try {
        	cfg.setClassForTemplateLoading(DocumentBuilder.class, "/");
            temp = cfg.getTemplate(template,"utf-8");
            StringWriter out = new StringWriter();
            try {
                temp.process(model, out);
            } catch (TemplateException e) {
                throw new RuntimeException(e);
            }
            out.flush();
            return out.getBuffer();
        } catch (IOException e) {
            throw new RuntimeException(e);
        } 
   }
   
   public static void buildPDF(StringBuffer fo, String pdf) throws IOException, FOPException {
       convertFO2PDF(fo.toString(), pdf);
   }
   
   public static void convertFO2PDF(String foString, String pdfFile) {
       OutputStream out = null;
       try {
           FOUserAgent foUserAgent = fopFactory.newFOUserAgent();
           File file = new File(pdfFile);
           file.getParentFile().mkdirs();
           out = new BufferedOutputStream(new FileOutputStream(file));
           Fop fop = fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out);
           // Setup JAXP using identity transformer
           SAXParserFactory spf = SAXParserFactory.newInstance();
           spf.setNamespaceAware(true);
           spf.setValidating(false);
           SAXParser saxParser = spf.newSAXParser();
           XMLReader xmlReader = saxParser.getXMLReader();
           xmlReader.setEntityResolver(new EntityResolver() {
			public InputSource resolveEntity(String publicId, String systemId)
					throws SAXException, IOException {
				// TODO Auto-generated method stub
				return null;
			}
			});
           TransformerFactory factory = TransformerFactory.newInstance();
           Transformer transformer = factory.newTransformer(); // identity transformer
           InputSource inputSource = new InputSource(new StringReader(foString));
           SAXSource source = new SAXSource(xmlReader, inputSource);
           // Setup input stream
           //Source src = new StreamSource(new StringReader(foString));
           // Resulting SAX events (the generated FO) must be piped through to FOP
           DefaultHandler fopHandler = fop.getDefaultHandler();
           Result res = new SAXResult(fopHandler);
           // Start XSLT transformation and FOP processing
           //transformer.transform(source, res);
           xmlReader.setContentHandler(fopHandler);
           xmlReader.parse(inputSource);
           //saxParser.parse(inputSource, fopHandler);
           // Result processing
           FormattingResults foResults = fop.getResults();
           System.out.println("Generated " + foResults.getPageCount() + " pages in total to "+file.getAbsolutePath());
       } catch (Exception e) {
           throw new RuntimeException(e);
       } finally {
           try {
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
       }
   }


}

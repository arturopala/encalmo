package org.opala.s3.fop;


import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

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
    private static FopFactory FOP_FACTORY = FopFactory.newInstance();
    private static Map<String,String> LOCAL_DTD_MAP = new HashMap<String,String>();
	private static SAXParserFactory SAX_PARSER_FACTORY;

    static{
        cfg = new Configuration();
        cfg.setClassForTemplateLoading(DocumentBuilder.class, "/ftl/");
        cfg.setObjectWrapper(new DefaultObjectWrapper());
        
        DefaultConfigurationBuilder fopcfgBuilder = new DefaultConfigurationBuilder();
        org.apache.avalon.framework.configuration.Configuration fopcfg;
        try {
            fopcfg = fopcfgBuilder.build(DocumentBuilder.class.getResourceAsStream("/fop.xconf"));
            FOP_FACTORY.setUserConfig(fopcfg);
            FOP_FACTORY.setBaseURL(DocumentBuilder.class.getResource("/").toExternalForm());
            FOP_FACTORY.setFontBaseURL(DocumentBuilder.class.getResource("/fonts/").toExternalForm());
        } 
        catch(RuntimeException re){
            throw re;
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        } 
        
        LOCAL_DTD_MAP.put("-//W3C//DTD MathML 2.0//EN", "/dtd/mathml2.dtd");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES MathML 2.0 Qualified Names 1.0//EN","/dtd/mathml2-qname-1.mod");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Arrow Relations for MathML 2.0//EN","/dtd/iso9573-13/isoamsa.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Binary Operators for MathML 2.0//EN","/dtd/iso9573-13/isoamsb.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Delimiters for MathML 2.0//EN","/dtd/iso9573-13/isoamsc.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Negated Relations for MathML 2.0//EN","/dtd/iso9573-13/isoamsn.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Ordinary for MathML 2.0//EN","/dtd/iso9573-13/isoamso.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Math Symbols: Relations for MathML 2.0//EN","/dtd/iso9573-13/isoamsr.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Greek Symbols for MathML 2.0//EN","/dtd/iso9573-13/isogrk3.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Math Alphabets: Fraktur for MathML 2.0//EN","/dtd/iso9573-13/isomfrk.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Math Alphabets: Open Face for MathML 2.0//EN","/dtd/iso9573-13/isomopf.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Math Alphabets: Script for MathML 2.0//EN","/dtd/iso9573-13/isomscr.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES General Technical for MathML 2.0//EN","/dtd/iso9573-13/isotech.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Box and Line Drawing for MathML 2.0//EN","/dtd/iso8879/isobox.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Russian Cyrillic for MathML 2.0//EN","/dtd/iso8879/isocyr1.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Non-Russian Cyrillic for MathML 2.0//EN","/dtd/iso8879/isocyr2.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Diacritical Marks for MathML 2.0//EN","/dtd/iso8879/isodia.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Latin 1 for MathML 2.0//EN","/dtd/iso8879/isolat1.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Added Latin 2 for MathML 2.0//EN","/dtd/iso8879/isolat2.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Numeric and Special Graphic for MathML 2.0//EN","/dtd/iso8879/isonum.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Publishing for MathML 2.0//EN","/dtd/iso8879/isopub.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Extra for MathML 2.0//EN","/dtd/mathml/mmlextra.ent");
        LOCAL_DTD_MAP.put("-//W3C//ENTITIES Aliases for MathML 2.0//EN","/dtd/mathml/mmlalias.ent");
        
        SAX_PARSER_FACTORY = SAXParserFactory.newInstance();
        SAX_PARSER_FACTORY.setNamespaceAware(true);
        SAX_PARSER_FACTORY.setValidating(false);
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
           FOUserAgent foUserAgent = FOP_FACTORY.newFOUserAgent();
           File file = new File(pdfFile);
           file.getParentFile().mkdirs();
           out = new BufferedOutputStream(new FileOutputStream(file));
           Fop fop = FOP_FACTORY.newFop(MimeConstants.MIME_PDF, foUserAgent, out);
           SAXParser saxParser = SAX_PARSER_FACTORY.newSAXParser();
           XMLReader xmlReader = saxParser.getXMLReader();
           InputSource inputSource = new InputSource(new StringReader(foString));
           DefaultHandler fopHandler = fop.getDefaultHandler();
           xmlReader.setContentHandler(fopHandler);
           xmlReader.setEntityResolver(new EntityResolver() {
				public InputSource resolveEntity(String publicId, String systemId)
							throws SAXException, IOException {
					String localPath = LOCAL_DTD_MAP.get(publicId);
					if(localPath!=null){
						return new InputSource(this.getClass().getResourceAsStream(localPath));
					}else{
						try {
							return new InputSource(new URL(systemId).openStream());
						} catch (Exception e) {
							throw new RuntimeException(e);
						}
					}
				}}
           );
           xmlReader.parse(inputSource);
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

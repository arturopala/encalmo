package org.encalmo.chart

import java.text.MessageFormat
import java.io.File
import scalax.io.Resource
import scalax.file.Path

/** 
 * JOGL native libraries initializer based on http://devblog.iscpif.fr/?p=76
 */
object JOGL {
   
   val libs = Seq("jogl_cg", "jogl_awt", "jogl", "gluegen-rt")
   
   val allNativeLibInfo = Seq(
        NativeLibInfo("win", "x86", "windows-i586", "", ".dll"),
        NativeLibInfo("win", "amd64", "windows-amd64", "", ".dll"),
        NativeLibInfo("win", "x86_64", "windows-amd64", "", ".dll"),
        NativeLibInfo("mac", "ppc", "macosx-ppc", "lib", ".jnilib"),
        NativeLibInfo("mac", "i386", "macosx-universal", "lib", ".jnilib"),
        NativeLibInfo("mac", "x86_64", "macosx-universal", "lib", ".jnilib"),
        NativeLibInfo("linux", "i386", "linux-i586", "lib", ".so"),
        NativeLibInfo("linux", "x86", "linux-i586", "lib", ".so"),
        NativeLibInfo("linux", "amd64", "linux-amd64", "lib", ".so"),
        NativeLibInfo("linux", "x86_64", "linux-amd64", "lib", ".so"),
        NativeLibInfo("sunos", "sparc", "solaris-sparc", "lib", ".so"),
        NativeLibInfo("sunos", "sparcv9", "solaris-sparcv9", "lib", ".so"),
        NativeLibInfo("sunos", "x86", "solaris-i586", "lib", ".so"),
        NativeLibInfo("sunos", "amd64", "solaris-amd64", "lib", ".so"),
        NativeLibInfo("sunos", "x86_64", "solaris-amd64", "lib", ".so")
    )
    
    val osName = System.getProperty("os.name");
    val osArch = System.getProperty("os.arch");
    val info = allNativeLibInfo.find(nli => nli.matchesOSAndArch(osName,osArch)).getOrElse(
            throw new IllegalStateException("Init failed : Unsupported os / arch ( " + osName + " / " + osArch + "). Please check you're using a 32-bit JVM.")
    )
    val libDir = extractLibDir(info)
    
    loadNatives(libDir,info)
    
    def extractLibDir(nativeLibInfo:NativeLibInfo):File = {
        val dir = new File(System.getProperty("java.io.tmpdir"),"JOGL")
        libs.foreach(lib => {
            val path = "/jogl/" + nativeLibInfo.getSubDirectoryPath + '/' + nativeLibInfo.getNativeLibName(lib);
            val is = classOf[NativeLibInfo].getResourceAsStream(path)
            val dest = Path(dir) / nativeLibInfo.getNativeLibName(lib)
            if(!dest.exists){
            	Resource.fromInputStream(is).copyData(dest)
            	Console.println("Lib "+lib+" copied.")
            }
        })
    dir
    }
    
    def loadNatives(dir:File,info:NativeLibInfo) = {
        try{
	        // disable JOGL and GlueGen runtime library loading from elsewhere
	        com.sun.opengl.impl.NativeLibLoader.disableLoading();
	        com.sun.gluegen.runtime.NativeLibLoader.disableLoading();
	        // Open GlueGen runtime library optimistically. Note that
	        // currently we do not need this on any platform except X11
	        // ones, because JOGL doesn't use the GlueGen NativeLibrary
	        // class anywhere except the DRIHack class, but if for
	        // example we add JOAL support then we will need this on
	        // every platform.
	        loadLibrary(dir, "gluegen-rt", info);
	        var driHackClass:Class[_] = null;
	        if (info.mayNeedDRIHack) {
	            // Run the DRI hack
	            driHackClass = Class.forName("com.sun.opengl.impl.x11.DRIHack");
	            try{
    	            val beginMethod = driHackClass.getMethod("begin")
    	            if(beginMethod!=null){
    	                beginMethod.invoke(null)
    	            }
	            }
	            catch { case _ => }
    	
	        }
	        // Load core JOGL native library
	        loadLibrary(dir, "jogl", info);
	        if (info.mayNeedDRIHack) {
	            // End DRI hack
	            try{
    	            val endMethod = driHackClass.getMethod("end")
    	            if(endMethod!=null){
    	                endMethod.invoke(null)
    	            }
    	        }
	            catch { case _ => }
	        }
	        if (!info.isMacOS) {
	            // borrowed from NativeLibLoader
	            // Must pre-load JAWT on all non-Mac platforms to
	            // ensure references from jogl_awt shared object
	            // will succeed since JAWT shared object isn't in
	            // default library path
	            try {
	                System.loadLibrary("jawt");
	                //Thread.sleep(3000)
	                Console.println("JAWT loaded.")
	            } catch {
	            	case e => 
	                // Accessibility technologies load JAWT themselves; safe to continue
	                // as long as JAWT is loaded by any loader
	                if (e.getMessage().indexOf("already loaded") == -1) {
	                    Console.println("Impossible to load JAWT. "+e.getClass.getName+": "+e.getMessage)
	                    throw e
	                }
	            }
	        }
	        // Load AWT-specific native code
	        loadLibrary(dir, "jogl_awt", info);
	        //Thread.sleep(1000)
        }
        catch {
            case e => {
                e.printStackTrace
                throw e
            }
        }
	}

    def loadLibrary(installDir:File, libName:String, nativeLibInfo:NativeLibInfo) = {
        val nativeLibName = nativeLibInfo.getNativeLibName(libName);
        try {
            System.load(new File(installDir, nativeLibName).getPath());
            Console.println("Native library "+libName+" loaded.")
        } catch {
            case ule:UnsatisfiedLinkError => {
	            // should be safe to continue as long as the native is loaded by any loader
	            if (ule.getMessage().indexOf("already loaded") == -1) {
	                Console.println("Unable to load " + nativeLibName + ". "+ule.getClass.getName+": "+ule.getMessage);
	                throw ule
	            }
	        }
        }
    }
    
    Console.println("JOGL init done.")
   
}

case class NativeLibInfo (
      osName:String,
      osArch:String,
      osNameAndArchPair:String,
      nativePrefix:String,
      nativeSuffix:String
){
   
   def matchesOSAndArch(osName:String, osArch:String):Boolean = {
        if (osName.toLowerCase().startsWith(this.osName)) {
             if ((this.osArch == null) || (osArch.toLowerCase().equals(this.osArch))) {
                    return true
             }
        }
        false
   }
   
   def matchesNativeLib(fileName:String):Boolean  = {
        if (fileName.toLowerCase().endsWith(nativeSuffix)) {
            true
        }
        false
   }
   
   def formatNativeJarName(nativeJarPattern:String):String = MessageFormat.format(nativeJarPattern, Seq(osNameAndArchPair))
   
   def getNativeLibName(baseName:String):String = nativePrefix + baseName + nativeSuffix;

   def isMacOS:Boolean = osName.equals("mac")
	
   def mayNeedDRIHack:Boolean =  !isMacOS && !osName.equals("win")
	
   def getSubDirectoryPath:String =  osNameAndArchPair
	   

	   
}
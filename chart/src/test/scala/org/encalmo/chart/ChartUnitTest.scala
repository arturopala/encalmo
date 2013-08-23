package org.encalmo.chart

import org.junit.{Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import java.io.File
import java.io.IOException
import java.util.logging.Level
import java.util.logging.Logger
import javax.imageio.ImageIO
import org.jzy3d.chart.controllers.keyboard.ChartKeyController
import org.jzy3d.colors.Color
import org.jzy3d.colors.ColorMapper
import org.jzy3d.colors.colormaps.ColorMapHotCold
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.contour.DefaultContourColoringPolicy
import org.jzy3d.contour.MapperContourPictureGenerator
import org.jzy3d.factories.JzyFactories
import org.jzy3d.maths.BoundingBox3d
import org.jzy3d.maths.Coord3d
import org.jzy3d.maths.Range
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.builder.Mapper
import org.jzy3d.plot3d.builder.concrete.OrthonormalGrid
import org.jzy3d.plot3d.primitives.Shape
import org.jzy3d.plot3d.primitives.axes.AxeFactory
import org.jzy3d.plot3d.primitives.axes.ContourAxeBox
import org.jzy3d.plot3d.primitives.axes.IAxe
import org.jzy3d.plot3d.rendering.canvas.ICanvas
import org.jzy3d.plot3d.rendering.canvas.Quality
import org.jzy3d.plot3d.rendering.legends.colorbars.ColorbarLegend
import org.jzy3d.plot3d.rendering.view.modes.CameraMode
import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
import org.jzy3d.plot3d.rendering.view.modes.ViewPositionMode
import scalax.file.Path
import org.jzy3d.plot3d.builder.concrete.SphereScatterGenerator
import java.util.Random

/** Tests chart generation with jyz3d */
@Ignore
class ChartUnitTest {
    
    // JOGL initialization
    JOGL
    
    @Test def test1:Unit = {
        // Define a function to plot
		val mapper:Mapper = new Mapper {
			override def f(x:Double, y:Double):Double = {
				x * Math.sin( x * y )
			}
		}

		// Define range and precision for the function to plot
		val range:Range = new Range(-4,4)
		val steps:Int = 80

		// Create the object to represent the function over the given range.
		val surface:Shape = Builder.buildOrthonormal(new OrthonormalGrid(range, steps, range, steps), mapper).asInstanceOf[Shape]
		val myColorMapper:ColorMapper = new ColorMapper(new ColorMapHotCold(), surface.getBounds().getZmin(), surface.getBounds().getZmax(), new Color(1,1,1,.5f))
		surface.setColorMapper(myColorMapper)
		surface.setFaceDisplayed(true)
		surface.setWireframeDisplayed(false);
		surface.setWireframeColor(Color.BLACK)

		// Create a chart with contour axe box, and attach the contour picture
		JzyFactories.axe = new AxeFactory {
			@Override
			override def getInstance:IAxe = {
				new ContourAxeBox(box);
			}
		};
		
		// Create a chart 
		val chart = new org.jzy3d.chart.Chart(Quality.Advanced,"offscreen,1200,1200")
		chart.getView().setAxeBoxDisplayed(true);
		chart.getView().setCameraMode(CameraMode.ORTHOGONAL);
		chart.getView().setBoundMode(ViewBoundMode.AUTO_FIT);
		chart.getView().setViewPositionMode(ViewPositionMode.FREE);
		chart.getView().setViewPoint(new Coord3d(0,java.lang.Math.PI/2,5));
		
		val cab = chart.getView().getAxe().asInstanceOf[ContourAxeBox]
		val contour = new MapperContourPictureGenerator(mapper, range, range);
		cab.setContourImg( contour.getFilledContourImage(new DefaultContourColoringPolicy(myColorMapper), 800, 800, 10), range, range);

		// Setup a colorbar for the surface object and add it to the scene
		chart.getScene().getGraph().add(surface);
		//ColorbarLegend cbar = new ColorbarLegend(surface, chart.getView().getAxe().getLayout());
		//surface.setLegend(cbar);
		//chart.addController( new ChartKeyController());
		createScreenshot(chart.getCanvas(),"ColorWaveDemo", 0);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/2,0,5));
        createScreenshot(chart.getCanvas(),"ColorWaveDemo", 1);
        chart.getView().setViewPoint(new Coord3d(0,0,5));
        createScreenshot(chart.getCanvas(),"ColorWaveDemo", 2);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/4,java.lang.Math.PI/4,5));
        createScreenshot(chart.getCanvas(),"ColorWaveDemo", 3);
    }
    
    @Test def test2:Unit = {
        
        val coords = SphereScatterGenerator.generate(new Coord3d(10,20,30),10000,20, true)
        val surface:Shape = Builder.buildDelaunay(coords).asInstanceOf[Shape]
        val myColorMapper:ColorMapper = new ColorMapper(new ColorMapHotCold(), surface.getBounds().getZmin(), surface.getBounds().getZmax(), new Color(1,1,1,.5f))
        surface.setColorMapper(myColorMapper)
        surface.setFaceDisplayed(true)
        surface.setWireframeDisplayed(false);
        surface.setWireframeColor(Color.BLACK)
        
        // Create a chart 
        val chart = new org.jzy3d.chart.Chart(Quality.Advanced,"offscreen,1200,1200")
        chart.getView().setAxeBoxDisplayed(true);
        chart.getView().setCameraMode(CameraMode.ORTHOGONAL);
        chart.getView().setBoundMode(ViewBoundMode.AUTO_FIT);
        chart.getView().setViewPositionMode(ViewPositionMode.FREE);
        chart.getView().setViewPoint(new Coord3d(0,java.lang.Math.PI/2,5));

        // Setup a colorbar for the surface object and add it to the scene
        chart.getScene().getGraph().add(surface);
        //ColorbarLegend cbar = new ColorbarLegend(surface, chart.getView().getAxe().getLayout());
        //surface.setLegend(cbar);
        //chart.addController( new ChartKeyController());
        createScreenshot(chart.getCanvas(),"SphericDelaunaySurface", 0);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/2,0,5));
        createScreenshot(chart.getCanvas(),"SphericDelaunaySurface", 1);
        chart.getView().setViewPoint(new Coord3d(0,0,5));
        createScreenshot(chart.getCanvas(),"SphericDelaunaySurface", 2);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/4,java.lang.Math.PI/4,5));
        createScreenshot(chart.getCanvas(),"SphericDelaunaySurface", 3);
    }
    
    @Test def test3:Unit = {
        
        import scala.collection.JavaConversions._
        
        val r = new Random();
        
        def coords:Seq[Coord3d] = for(x <- 0 to 1000) yield {
            var x = 2.0*r.nextDouble();
            var y = 2.0*r.nextDouble();
            val s = r.nextDouble();            

            if (s>0.75) {x = x * -1; y = y * -1}
            if (s>0.5 && s<= 0.75) {x *= -1;}
            if (s>0.25 && s<= 0.5) {y *= -1 ;}            

            // exp( -(x**2 + y**2) ) * cos(x/4)*sin(y) * cos(2*(x**2+y**2))
            val f1 = Math.cos(2*(x*x+y*y));
            val f2 = Math.cos(x/4)*Math.sin(y);
            val f3 = x*x + y*y;
            val z = Math.pow(Math.E, -f3)*f2*f1;

            new Coord3d(x,y,z)
        }
        
        val surface:Shape = Builder.buildDelaunay(coords).asInstanceOf[Shape]
        val myColorMapper:ColorMapper = new ColorMapper(new ColorMapRainbow(), surface.getBounds().getZmin(), surface.getBounds().getZmax(), new Color(1,1,1,.5f))
        surface.setColorMapper(myColorMapper)
        surface.setFaceDisplayed(true)
        surface.setWireframeDisplayed(true);
        surface.setWireframeColor(Color.BLACK)
        
        // Create a chart 
        val chart = new org.jzy3d.chart.Chart(Quality.Advanced,"offscreen,1200,1200")
        chart.getView().setAxeBoxDisplayed(true);
        chart.getView().setCameraMode(CameraMode.ORTHOGONAL);
        chart.getView().setBoundMode(ViewBoundMode.AUTO_FIT);
        chart.getView().setViewPositionMode(ViewPositionMode.FREE);
        chart.getView().setViewPoint(new Coord3d(0,java.lang.Math.PI/2,5));

        // Setup a colorbar for the surface object and add it to the scene
        chart.getScene().getGraph().add(surface);
        //ColorbarLegend cbar = new ColorbarLegend(surface, chart.getView().getAxe().getLayout());
        //surface.setLegend(cbar);
        //chart.addController( new ChartKeyController());
        createScreenshot(chart.getCanvas(),"GeneratedDelaunaySurface", 0);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/2,0,5));
        createScreenshot(chart.getCanvas(),"GeneratedDelaunaySurface", 1);
        chart.getView().setViewPoint(new Coord3d(0,0,5));
        createScreenshot(chart.getCanvas(),"GeneratedDelaunaySurface", 2);
        chart.getView().setViewPoint(new Coord3d(java.lang.Math.PI/4,java.lang.Math.PI/4,5));
        createScreenshot(chart.getCanvas(),"GeneratedDelaunaySurface", 3);
    }
    
    def createScreenshot(ic:ICanvas, name:String, id:Int) {
        val path = new File("target/screenshots",name+"_"+id+".png")
        if (!path.exists()) {
            path.mkdirs();
        }
        try {
            ImageIO.write(ic.screenshot(), "png", path);
            Console.println("Dumped screenshot in: " + path);
        } catch {
            case e: Throwable => {
                e.printStackTrace
                throw e
            }
        }
    }

}
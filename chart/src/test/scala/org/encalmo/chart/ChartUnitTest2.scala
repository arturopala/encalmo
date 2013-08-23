package org.encalmo.chart

import org.junit.{Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import java.io.File
import java.io.IOException
import java.util.logging.Level
import java.util.logging.Logger
import javax.imageio.ImageIO
import scalax.file.Path
import java.util.Random
import org.jzy3d.plot3d.builder.Mapper

/** Tests chart generation */
@Ignore
class ChartUnitTest2 {
    
    // JOGL initialization
    JOGL
    
    @Test def test1:Unit = {
        // Define a function to plot
		val mapper:Mapper = new Mapper {
			override def f(x:Double, y:Double):Double = {
				x * Math.sin( x * y )
			}
		}
		val steps:Int = 80
		val data = for(x <- -4d to 4d by 0.1d; y <- -4d to 4d by 0.1d) yield (x,y,mapper.f(x,y))
		val chart = Chart(data)
		chart.writeImage(Path("target/test1.png"),1200,1200,ChartOptions(view=5,wireframeDisplayed=false))
    }

}
package org.encalmo.chart
import org.jzy3d.maths.Coord3d
import org.jzy3d.plot3d.primitives.Shape
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.rendering.canvas.Quality
import scalax.file.Path
import org.jzy3d.plot3d.rendering.view.modes.CameraMode
import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
import org.jzy3d.plot3d.rendering.view.modes.ViewPositionMode
import org.jzy3d.plot3d.rendering.canvas.ICanvas
import javax.imageio.ImageIO
import java.io.File
import org.jzy3d.colors.ColorMapper
import org.jzy3d.colors.colormaps.ColorMapHotCold
import org.jzy3d.colors.Color
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.colors.colormaps.ColorMapRedAndGreen
import org.jzy3d.colors.colormaps.ColorMapWhiteBlue
import org.jzy3d.colors.colormaps.ColorMapWhiteGreen
import org.jzy3d.colors.colormaps.ColorMapWhiteRed
import org.jzy3d.colors.colormaps.ColorMapGrayscale

/** Chart */
case class Chart (
        /** Chart data sequence */
        data:Seq[(Double,Double,Double)]
) {
    
    import scala.collection.JavaConversions._
    
    /** Jzy3d surface coordinates*/
    lazy val coords:Seq[Coord3d] = data.map(d => new Coord3d(d._1,d._2,d._3))
    lazy val surface:Shape = Builder.buildDelaunay(coords).asInstanceOf[Shape]
    
    def writeImage(path:Path,width:Int = 1024,height:Int = 1024,options:ChartOptions = ChartOptions()) = {
        val max = Math.max(surface.getBounds.getZmin,surface.getBounds.getZmax)
        val myColorMapper:ColorMapper = new ColorMapper(options.colorMap, -max, max, new Color(1,1,1,.5f))
        surface.setColorMapper(myColorMapper)
        surface.setFaceDisplayed(options.faceDisplayed)
        surface.setWireframeDisplayed(options.wireframeDisplayed)
        surface.setWireframeColor(options.wireframeColor)
        val chart = new org.jzy3d.chart.Chart(Quality.Advanced,"offscreen,"+width+","+height+"")
        chart.getView.setAxeBoxDisplayed(options.axeBox)
        chart.getView.setCameraMode(options.cameraMode)
        chart.getView.setBoundMode(options.boundMode)
        chart.getView.setViewPositionMode(ViewPositionMode.FREE)
        chart.getView.setViewPoint(options.viewPoint)
        chart.getScene.getGraph.add(surface)
        createImageFile(chart,path)
    }
    
    private def createImageFile(chart:org.jzy3d.chart.Chart, path:Path) {
        try {
            val pathString = path.toAbsolute.path
            ImageIO.write(chart.getCanvas.screenshot(), "png", new File(pathString))
            Console.println("Image saved in: " + path)
        } catch {
            case e: Throwable => {
                e.printStackTrace()
                throw e
            }
        }
    }

}

/** Chart options */
case class ChartOptions (
        faceDisplayed:Boolean = true,
        wireframeDisplayed:Boolean = true,
        wireframeColor:Color = Color.BLACK,
        axeBox:Boolean = true,
        perspective:Boolean = false,
        autoFit:Boolean = true,
        view:Int = 5,
        distance:Double = 5,
        /** HotCold|Rainbow|RedAndGreen|WhiteBlue|WhiteRed|WhiteGreen|Grayscale */
        colors:String = "HotCold"
){
    
    import ChartOptions._
    
    lazy val cameraMode = if (perspective) CameraMode.PERSPECTIVE else CameraMode.ORTHOGONAL
    lazy val boundMode = if (autoFit) ViewBoundMode.AUTO_FIT else ViewBoundMode.MANUAL
    lazy val viewPoint = view match {
        // box walls
        case 1 => new Coord3d(0,0,distance)
        case 2 => new Coord3d(PI2,0,distance)
        case 3 => new Coord3d(2*PI2,0,distance)
        case 4 => new Coord3d(3*PI2,0,distance)
        case 5 => new Coord3d(0,PI2,distance)
        case 6 => new Coord3d(0,-PI2,distance)
        // box edges
        case 12 => new Coord3d(PI4,0,distance)
        case 23 => new Coord3d(PI2+PI4,0,distance)
        case 34 => new Coord3d(2*PI2+PI4,0,distance)
        case 41 => new Coord3d(3*PI2+PI4,0,distance)
        case 51 => new Coord3d(0,PI4,distance)
        case 52 => new Coord3d(PI2,PI4,distance)
        case 53 => new Coord3d(2*PI2,PI4,distance)
        case 54 => new Coord3d(3*PI2,PI4,distance)
        case 61 => new Coord3d(0,-PI4,distance)
        case 62 => new Coord3d(PI2,-PI4,distance)
        case 63 => new Coord3d(2*PI2,-PI4,distance)
        case 64 => new Coord3d(3*PI2,-PI4,distance)
        //box corners
        case 512 => new Coord3d(PI4,PI4,distance)
        case 523 => new Coord3d(PI2+PI4,PI4,distance)
        case 534 => new Coord3d(2*PI2+PI4,PI4,distance)
        case 541 => new Coord3d(3*PI2+PI4,PI4,distance)
        case 612 => new Coord3d(PI4,-PI4,distance)
        case 623 => new Coord3d(PI2+PI4,-PI4,distance)
        case 634 => new Coord3d(2*PI2+PI4,-PI4,distance)
        case 641 => new Coord3d(3*PI2+PI4,-PI4,distance)
        //default: top wall
        case _ => new Coord3d(0,PI2,distance)
    }
    lazy val colorMap = colors match {
        case "HotCold" => new ColorMapHotCold()
        case "Rainbow" => new ColorMapRainbow()
        case "RedAndGreen" => new ColorMapRedAndGreen()
        case "WhiteBlue" => new ColorMapWhiteBlue()
        case "WhiteGreen" => new ColorMapWhiteGreen()
        case "WhiteRed" => new ColorMapWhiteRed()
        case "Grayscale" => new ColorMapGrayscale()
    }
    
}

object ChartOptions {
    
    val PI2 = java.lang.Math.PI/2
    val PI4 = java.lang.Math.PI/4
    
}


package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Future
import org.encalmo.structures.miscellaneous.AOMTWorksheet
import org.encalmo.printer.{HtmlOutputPreferences, HtmlOutput}
import org.encalmo.style.PredefinedStyles

object Application extends Controller {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def multistoreycarpark = Action.async {
    val f = Future[String] {
      val layout = PredefinedStyles.layout
      val prefs: HtmlOutputPreferences = HtmlOutputPreferences().asFragmentOfPage().ignoreDocumentStyles(true)
      val output: HtmlOutput = new HtmlOutput(layout, new java.util.Locale("PL"), prefs)
      new AOMTWorksheet().renderHtml(output)
      output.getResult
    }
    f map {
      case result =>  Ok(views.html.multistoreycarpark(result))
    }
  }

}
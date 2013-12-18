package helper

object EncalmoFormHelpers {
  import views.html.helper.FieldConstructor
  implicit val myFields = FieldConstructor(views.html.input.apply)
}
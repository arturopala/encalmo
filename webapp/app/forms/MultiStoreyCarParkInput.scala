package forms

import play.api.data._
import play.api.data.Forms._

case class MultiStoreyCarParkInput(
      beamLength: BigDecimal,
      slabSpanWidth: BigDecimal,
      slabTotalHeight: BigDecimal,
      load: BigDecimal,
      mountingSupportsNumber: Int
)

object MultiStoreyCarParkInput {
  val form = Form[MultiStoreyCarParkInput](
    mapping(
      "beamLength" -> bigDecimal,
      "slabSpanWidth" -> bigDecimal,
      "slabTotalHeight" -> bigDecimal,
      "load" ->  bigDecimal,
      "mountingSupportsNumber" -> default(number,0)
    )(MultiStoreyCarParkInput.apply)(MultiStoreyCarParkInput.unapply)
  )
}

package org.encalmo.document

import org.encalmo.style.Style

/**
 * Document component representing image
 * @author artur.opala
 */
case class Image(override val customStyleOfComponent: Option[Style], source:String)
extends DocumentComponent(customStyleOfComponent) {
    

}

object Image {

    def apply(source:String): Image = {
        new Image(None,source)
    }

    def apply(customStyle: Style, source:String): Image = {
        new Image(Option(customStyle),source)
    }

}


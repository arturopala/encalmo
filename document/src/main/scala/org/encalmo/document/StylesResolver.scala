package org.encalmo.document

import org.encalmo.style.{StylesConfigSymbols, Style, StylesConfig}

trait StylesResolver {

    def parentStylesConfig:Option[StylesConfig]

    /**
     * Resolves style
     */
    final def resolveExpressionStyle(defaultStyle:Style, part:StylesConfigSymbols.Value):Style = {
        Option(defaultStyle).getOrElse(
            parentStylesConfig match {
                case Some(psc) => psc.part(part).getOrElse(psc.expression.getOrElse(null))
                case None => null
            }
        )
    }

}

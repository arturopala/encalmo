package org.encalmo.document

import org.encalmo.style.{StylesConfigSymbols, Style, StylesConfig}

trait StylesResolver {

    def stylesConfig:StylesConfig

    /**
     * Resolves style
     */
    final def resolveExpressionStyle(defaultStyle:Style, part:StylesConfigSymbols.Value):Style = {
        Option(defaultStyle).getOrElse(stylesConfig.part(part))
    }

}

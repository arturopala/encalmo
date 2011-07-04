package org.encalmo.fea
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

/** Double numbers formatting */
object DoubleFormat {
    
    lazy val SHORT =  new DecimalFormat("0.000000", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
    
    def short(d:Double):String = SHORT.format(d)

}
package org.encalmo.expression

/**
 * Formatted number container
 * @author artur.opala
 */
case class NumberFormatted(
	isNegative:Boolean,
	hasExponent:Boolean,
	integer:Long,
	fraction:Double,
	exponent:Int,
	decimals:Int,
	unit:UnitOfValue
)

object NumberFormatted {

    def formatForPrint(number: Number):NumberFormatted = {

        def analyze(d:Double):(Long,Double) = {
            val ad = Math.abs(d)
            val fd = Math.floor(ad).toLong
            val fr = ad-fd
            if(fr+0.0001>=1) {
                (fd+1,0)
            } else {
                (fd,ad-fd)
            }
        }

        def getScale(d:Double):Int = java.lang.Math.log10(d).toInt

        val rif = analyze(number.r.d)
        val scaleOfInteger:Int = if(rif._1==0) 0 else getScale(rif._1)
        val scaleOfFraction:Int = if(rif._2==0) 0 else Math.abs(getScale(rif._2))
        if(scaleOfInteger>6){
            val nsi = scaleOfInteger % 3
            val nrif = analyze(rif._1/Math.pow(10,scaleOfInteger-nsi))
            NumberFormatted(
                number.r.isNegative,
                true,
                nrif._1,
                nrif._2,
                scaleOfInteger-nsi,
                2,
                number.unit
            )
        }else{
            if(scaleOfInteger==0){
                if(rif._1>0){
                    NumberFormatted(
                        number.r.isNegative,
                        false,
                        rif._1,
                        rif._2,
                        0,
                        3,
                        number.unit
                    )
                }else{
                    if(scaleOfFraction>3){
                        val newScaleOfFraction = (scaleOfFraction/3)*3+3
                        val nrif = analyze(rif._2*Math.pow(10,newScaleOfFraction))
                        if(nrif._2==0 && scaleOfFraction%3d==0){
                            val n2rif = analyze(rif._2*Math.pow(10,newScaleOfFraction-3))
                            NumberFormatted(
                                number.r.isNegative,
                                true,
                                n2rif._1,
                                n2rif._2,
                                -(newScaleOfFraction-3),
                                3,
                                number.unit
                            )
                        }else{
                            NumberFormatted(
                                number.r.isNegative,
                                true,
                                nrif._1,
                                nrif._2,
                                -newScaleOfFraction,
                                3,
                                number.unit
                            )
                        }
                    }else{
                        NumberFormatted(
                            number.r.isNegative,
                            false,
                            0,
                            rif._2,
                            0,
                            3,
                            number.unit
                        )
                    }
                }
            }else{
                if(scaleOfInteger+scaleOfFraction>6){
                    NumberFormatted(
                        number.r.isNegative,
                        false,
                        rif._1,
                        0,
                        0,
                        0,
                        number.unit
                    )
                }else{
                    NumberFormatted(
                        number.r.isNegative,
                        false,
                        rif._1,
                        rif._2,
                        0,
                        2,
                        number.unit
                    )
                }
            }
        }
    }

}
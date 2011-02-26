package org.encalmo.expression

/**
 * RoundingMode trait
 * @author artur.opala
 */
trait RoundingMode {
	
	def round(d:Double):Double

}

/**
 * Rounding modes
 * @author artur.opala
 */
object RoundingMode {
	
	lazy val CEILING = new RoundingMode{
		def round(d:Double):Double = {
			Math.ceil(d)
		}
	}
	
	lazy val FLOOR = new RoundingMode{
		def round(d:Double):Double = {
			Math.floor(d)
		}
	}

	lazy val HALF = new RoundingMode{
		def round(d:Double):Double = {
			Math.floor(d+0.5)
		}
	}
	
	object Sequence{
	
		def apply(up:Boolean,vals:Seq[Double]):RoundingMode = new RoundingMode{
			def round(d:Double):Double = {
				if(!vals.isEmpty){
					if(up){
						val d2 = vals.filter(_ > d)
						if(d2.isEmpty){
							d
						}else{
							d2.first
						}
					}else{
						val d1 = vals.filter(_ < d)
						if(d1.isEmpty){
							d
						}else{
							d1.last
						}
					}
				}else{
					d
				}
			}
		}
	}
	
	object Step {
		
		def apply(up:Boolean,step:Double):RoundingMode = new RoundingMode{
			def round(d:Double):Double = {
				if(step!=0){
					if(up){
						Math.ceil(d/step)*step
					}else{
						Math.floor(d/step)*step
					}
				}else{
					d
				}
			}
		}
		
	}
	
}
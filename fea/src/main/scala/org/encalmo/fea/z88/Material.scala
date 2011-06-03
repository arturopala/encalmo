package org.encalmo.fea.z88

/**
 * Material information
 */
trait Material {
    
    val E:Double //Young’s Modulus
    val P:Double //Poisson's Ratio

}

object Material{
    
    def apply(vE:Double,vP:Double) = new Material {
        val E:Double = vE
        val P:Double = vP
    }
    
}



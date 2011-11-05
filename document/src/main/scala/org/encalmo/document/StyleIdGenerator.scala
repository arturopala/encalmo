package org.encalmo.document

object StyleIdGenerator {
    
    val SET = "abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ"
    
    def apply():String = (for (x <- 0 to 6) yield SET.charAt((Math.random*SET.length()).asInstanceOf[Int])).mkString
    
}
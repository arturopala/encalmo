package org.encalmo.document

object StyleClassIdGenerator {
    
    val SET = "abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ"
    
    def apply():String = "z"+(for (x <- 0 to 6) yield SET.charAt((Math.random*SET.length()).asInstanceOf[Int])).mkString
    
}
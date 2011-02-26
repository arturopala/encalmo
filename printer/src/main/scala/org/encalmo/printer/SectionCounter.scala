package org.encalmo.printer

import org.encalmo.document._

/**
 * Section counter stategy
 * @author artur.opala
 */
class SectionCounter(val enumerator:Enumerator) {
	
	var counters:Seq[Counter] = Seq(Counter())
	var currentLevel:Int = 0
	var currentCounter:Counter = counters(0)

	/**
	 * Returns current counter's state
	 */
	def current:Seq[String] = {
		for(x <- 0 to currentLevel) yield enumerator(x,counters(x).item)
	}
	
	def positions:Seq[Int] = {
        for(x <- 0 to currentLevel) yield counters(x).item
    }
	
	def string:String = enumerator.string(positions)
	
	/**
	 * Increase curent level
	 */
	def in:Unit = {
		currentLevel = currentLevel+1
		if(counters.size==currentLevel){
			counters = counters :+ Counter()
		}
		currentCounter = counters(currentLevel)
	}
	
	/**
	 * Decrease curent level
	 */
	def out:Unit = {
		for(x <- currentLevel to counters.size - 1){
			counters(x).reset
		}
		currentLevel = currentLevel-1
		if(currentLevel<0){
			currentLevel = 0
		}
		currentCounter = counters(currentLevel)
	}
	
	/**
	 * Increase current counter state
	 */
	def next:Unit = {
		currentCounter.increment
	}
	
}

/**
 * SectionCounter class companion object
 * @author artur.opala
 */
object SectionCounter {
	
	def apply():SectionCounter = new SectionCounter(Enumerator())
	
	def apply(e:Enumerator):SectionCounter = new SectionCounter(e)
	
}

/**
 * Counter class
 * @author artur.opala
 */
case class Counter(var item:Int = 1){
	
	def increment:Unit = {
		item = item + 1
	}
	
	def reset = {
		item = 1
	}
	
}
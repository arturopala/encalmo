package org.encalmo.document

/**
 * Multiple levels counter
 * @author artur.opala
 */
class MultiCounter(val enumerator:Enumerator, offset: Int = 1) {
	
	var counters:Seq[Counter] = Seq(Counter(offset))
	var currentLevel:Int = 0
	var currentCounter:Counter = counters(0)

    def copy(): MultiCounter = {
        val sc =new MultiCounter(enumerator, offset)
        sc.counters = for(c <- counters) yield c.copy()
        sc.currentLevel = currentLevel
        sc.currentCounter = sc.counters(sc.currentLevel)
        sc
    }

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
	def in():Unit = {
		currentLevel = currentLevel+1
		if(counters.size==currentLevel){
			counters = counters :+ Counter()
		}
		currentCounter = counters(currentLevel)
	}
	
	/**
	 * Decrease curent level
	 */
	def out():Unit = {
		for(x <- currentLevel to counters.size - 1){
			counters(x).reset()
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
	def next():Unit = {
		currentCounter.increment()
	}
	
}

/**
 * SectionCounter class companion object
 * @author artur.opala
 */
object MultiCounter {
	
	def apply():MultiCounter = new MultiCounter(Enumerator())
	
	def apply(e:Enumerator, offset: Int = 1):MultiCounter = new MultiCounter(e,offset)
	
}

/**
 * Counter class
 * @author artur.opala
 */
case class Counter(var item:Int = 1){
	
	def increment():Unit = {
		item = item + 1
	}
	
	def reset() = {
		item = 1
	}

    def copy(): Counter = new Counter(item)
	
}
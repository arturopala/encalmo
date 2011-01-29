package org.encalmo.document

/**
 * List style class
 * @author artur.opala
 */
case class ListStyle(
    distanceBetweenStarts:Int=10,
    distanceLabelSeparation:Int=2,
    bullet:String = "·",
    unit:String = "pt"
){
   
    def useDistanceBetweenStarts(d:Int):ListStyle = copy(distanceBetweenStarts = d)
    def useDistanceLabelSeparation(d:Int):ListStyle  = copy(distanceLabelSeparation = d)
    def useBullet(s:String):ListStyle = copy(bullet = s)
    
    def useUnit(u:String) = copy(unit = u)
    
}
    
/**
 * Default font style object
 * @author artur.opala
 */
object DefaultListStyle extends ListStyle()
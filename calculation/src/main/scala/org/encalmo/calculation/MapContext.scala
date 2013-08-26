package org.encalmo.calculation

import scala.collection.mutable
import org.encalmo.expression._

/** 
 * Base mutable and lockable context implementation
 */
class MapContext extends MutableContext {
	
	val map:mutable.Map[Symbol,Expression] = mutable.LinkedHashMap[Symbol,Expression]()

    protected var opened: Boolean = true
    def lock() = opened = false
    protected def throwContextAlreadyLockedException = throw new IllegalStateException("This context is locked.")

    /**
     * Map expression to the symbol in this context
     */
    override def update(symb: Symbol, expression: Expression): Unit = {
        if (opened) {
            map.put(symb, expression match {
                case v: Value => v.convertTo(symb.unit)
                case other => other
            })
        } else throwContextAlreadyLockedException
    }

    /** Maps expressions to the symbols in this context */
    override def put(ts: (Symbol, Expression)*): this.type = {
        if (opened) {
            for ((s, e) <- ts) {
                update(s, e)
            }
            this
        } else throwContextAlreadyLockedException
    }

    /**
     * Returns expression mapped to that symbol or None
     */
    override def getExpression(s:Symbol):Option[Expression] = getMappedExpression(s)

    protected final def getMappedExpression(s:Symbol):Option[Expression] = map.get(s)

    override def hasExpression(s:Symbol):Boolean = hasMappedExpression(s)

    protected final def hasMappedExpression(s:Symbol):Boolean = map.contains(s)

    override def listMappings: Seq[(Symbol,Expression)] = listMappedHere

    protected final def listMappedHere: Seq[(Symbol,Expression)] = map.toSeq

    override def listSymbols: Seq[Symbol] = listMappedSymbols

    protected final def listMappedSymbols: Seq[Symbol] = map.keySet.toSeq

    override def listNestedResolvers:Seq[Context] = Seq.empty[Context]
	
}

object MapContext {

	def apply() = new MapContext()

    def apply(vmap: Map[Symbol,Expression]) = new MapContext(){
        override val map: mutable.Map[Symbol,Expression] = mutable.Map() ++ vmap
    }

    def apply(entry:(Symbol,Expression)*) = new MapContext(){
        override val map = mutable.Map(entry:_*)
    }

}
package org.encalmo.calculation

import scala.collection.mutable
import scala.collection.Set
import org.encalmo.expression._

/** 
 * Base mutable and lockable context implementation
 */
class MapContext(val dictionary: Option[String] = None) extends MutableContext {

    def this(dict: String) = this(Option(dict))
	
	val map:mutable.Map[Symbol,Expression] = mutable.LinkedHashMap[Symbol,Expression]()

    protected var opened: Boolean = true
    def lock() = opened = false
    protected def throwContextAlreadyLockedException = throw new IllegalStateException("This context is locked.")

    /**
     * Map expression to the symbol in this context
     */
    override def update(symb: Symbol, expression: Expression): Unit = {
        if (opened) {
            expression match {
                case v: Value => map.put(symb,v.convertTo(symb.unit))
                case Annotated(e,description) => map.put(symb ## description,e)
                case other => map.put(symb,other)
            }

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
    override def listMappings(filter: ((Symbol,Expression))=>Boolean):Seq[(Symbol,Expression)] = listMappedHere(filter)

    protected final def listMappedHere: Seq[(Symbol,Expression)] = map.toSeq
    protected final def listMappedHere(filter: ((Symbol,Expression))=>Boolean): Seq[(Symbol,Expression)] = map.filter(filter).toSeq

    override def listSymbols: Set[Symbol] = listMappedSymbols
    override def listSymbols(filter: Symbol=>Boolean): Set[Symbol] = listMappedSymbols(filter)

    private[calculation] final def listMappedSymbols: Set[Symbol] = map.keySet
    private[calculation] final def listMappedSymbols(filter: Symbol=>Boolean): Set[Symbol] = map.keySet.filter(filter)

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
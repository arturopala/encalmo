package org.encalmo.calculation

import org.encalmo.expression.{Expression, Symbol}
import scala.collection.mutable

object ContextFactory {

    def apply(): MapContext = MapContext()

    def apply(vmap: Map[Symbol,Expression]) = new MapContext(){
        override val map: mutable.Map[Symbol,Expression] = mutable.Map() ++ vmap
    }

    def apply(entry:(Symbol,Expression)*) = new MapContext(){
        override val map = mutable.Map(entry:_*)
    }
}
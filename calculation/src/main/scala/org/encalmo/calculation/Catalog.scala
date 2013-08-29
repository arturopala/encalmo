package org.encalmo.calculation

abstract class Catalog[A](val name: String) {

    def map: Map[String,()=>A]
    def apply(id: String): A = map.get(id).map(x => x()).getOrElse(throw new IllegalArgumentException(s"Definition of $name $id not found!"))

}

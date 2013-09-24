package org.encalmo.document

import org.encalmo.style.Style

/**
 * TableOfContents section component class
 * @author artur.opala
 */
case class TableOfContents(title:String, levels: Int = 5, override val customStyleOfComponent: Option[Style] = None) extends DocumentComponent(customStyleOfComponent)
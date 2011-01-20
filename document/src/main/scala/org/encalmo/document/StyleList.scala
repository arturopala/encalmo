package org.encalmo.document

/**
 * Style list used for automatic NumSection styling
 * @author artur.opala
 */
case class StyleList(itemStyle:Style*)
extends DocumentComponent(null) with NonVisualDocumentComponent
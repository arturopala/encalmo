package org.encalmo.document
import org.encalmo.style.StylesConfig
import org.encalmo.style.DefaultStyle
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.mutable.ArrayBuffer
import org.encalmo.expression.Symbol
import org.encalmo.document.Document.SymbolCollectingDocumentTreeVisitor

/**
 * Document, root {@link DocumentComponent}
 * @author artur
 */
class Document(
        val title: String,
        val styles: StylesConfig,
        flow: DocumentComponent*)
extends DocumentComponentSeq(Option(styles.default), flow:_*) with BlockComponent {
	
	override def toString = "Document("+customStyle+","+title+","+flow.mkString(",")+")"

    def findAllExpressionSymbols():Set[Symbol] = {
        val collector = new SymbolCollectingDocumentTreeVisitor()
        this.visit(collector)
        collector.result
    }

}

/**
 * Document class companion object
 * @author artur.opala
 */
object Document {
	
	def apply(title:String, stylesConfig:StylesConfig, flow:DocumentComponent*) = {
		new Document(title, stylesConfig, flow:_*)
	}
	
	def apply(title:String, flow:DocumentComponent*) = {
		new Document(title, StylesConfig(DefaultStyle), flow:_*)
	}
	
	def apply(flow:DocumentComponent*) = {
		new Document("", StylesConfig(DefaultStyle), flow:_*)
	}

	def unapply(d:Document) = Some(d.customStyle,d.title,d.flow)

    class SymbolCollectingDocumentTreeVisitor extends TreeVisitor[DocumentComponent] {

        private val buffer = ArrayBuffer[Symbol]()
        def result:Set[Symbol] = buffer.toSet

        override def onEnter(node: Node[DocumentComponent]): Unit = node.element match {
            case ehc:ExpressionHolderComponent => {
                 ehc.expressions foreach {
                     case symbol: Symbol => buffer += symbol
                     case _ =>
                 }
            }
            case _ =>
        }
    }
	
}
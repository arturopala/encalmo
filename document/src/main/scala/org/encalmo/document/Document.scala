package org.encalmo.document
import org.encalmo.style.StylesConfig
import org.encalmo.style.DefaultStyle
import org.encalmo.common.{Node, TreeVisitor}
import scala.collection.mutable.ArrayBuffer
import org.encalmo.expression.{Assert, Symbol}
import org.encalmo.document.Document.{AssertionSymbolCollectingDocumentTreeVisitor, SymbolCollectingDocumentTreeVisitor}

/**
 * Document, root {@link DocumentComponent}
 * @author artur.opala
 */
class Document(
        val title: String,
        val styles: StylesConfig,
        flow: DocumentComponent*)
extends DocumentComponentSeq(Option(styles.default), flow:_*) with BlockComponent {
	
	override def toString = "Document("+customStyle+","+title+","+flow.mkString(",")+")"

    def findAllSymbols():Set[Symbol] = {
        val collector = new SymbolCollectingDocumentTreeVisitor()
        this.visit(collector)
        collector.result
    }

    def findAllAssertionSymbols():Set[Symbol] = {
        val collector = new AssertionSymbolCollectingDocumentTreeVisitor()
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

        protected val buffer = ArrayBuffer[Symbol]()
        def result:Set[Symbol] = buffer.toSet

        override def onEnter(node: Node[DocumentComponent]): Unit = node.element match {
            case ehc:ExpressionHolderComponent => {
                 ehc.expressions foreach {
                     case symbol: Symbol => buffer += symbol
                     case e =>  e.allNestedChildrenOfType[Symbol](classOf[Symbol]).foreach(buffer.+=)
                 }
            }
            case _ =>
        }
    }

    class AssertionSymbolCollectingDocumentTreeVisitor extends SymbolCollectingDocumentTreeVisitor {

        override def onEnter(node: Node[DocumentComponent]): Unit = node.element match {
            case a: Assertion => {
                a.expressions.foreach {
                    case symbol: Symbol => buffer += symbol
                    case e => e.allNestedChildrenOfType[Symbol](classOf[Symbol]).foreach(buffer.+=)
                }
            }
            case ehc:ExpressionHolderComponent => {
                ehc.expressions foreach {
                    case asrt: Assert => asrt.allNestedChildrenOfType[Symbol](classOf[Symbol]).foreach(buffer.+=)
                    case _ =>
                }
            }
            case _ =>
        }
    }
	
}
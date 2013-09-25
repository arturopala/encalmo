package org.encalmo.calculation

import org.encalmo.expression._
import java.util.UUID
import scala.annotation.tailrec
import org.encalmo.graph.Graph

/** 
 * Expression's calculation context trait
 */
trait Context extends SymbolConfigurator {
    
    val MAX_MAP_ALL_LOOP_COUNT:Int = 256

    final val id:String = UUID.randomUUID().toString
  
	/**
	 * Should return expression mapped to that symbol or None
	 */
	final def getExpression(s:Symbol, cache: ResultsCache):Option[Expression] = {
        cache.get(s) orElse getExpression(s)
    }
	
	/**
	 * Should return unresolved expression mapped to that symbol or None
	 */
	def getExpression(s:Symbol):Option[Expression]
	
	/**
	 * Should return true if exists expression mapped to that symbol
	 */
	def hasExpression(s:Symbol):Boolean
	
	/**
	 * Should return sequence of used mappings
	 */
	def listMappings:Seq[(Symbol,Expression)]
	
	/**
     * Should return sequence of mapped symbols
     */
    def listSymbols:Seq[Symbol]

    /**
     * Should return sequence of nested expression contexts
     */
    def listNestedResolvers:Seq[Context]

    def topologicallySortedSymbols: Seq[Symbol] = {
        val graph: Graph[Symbol] = SymbolGraph.build(this)
        Graph.sortTopologically(graph)
    }

    def topologicallySortedConnectedSymbols: Seq[Symbol] = {
        val graph: Graph[Symbol] = SymbolGraph.build(this)
        Graph.sortTopologically(graph).filter(graph.isConnected)
    }

    /** Returns expression pinned to the context */
    def apply(s:Symbol):PinnedExpression = PinnedExpression(this,s)

    def ifValue(expression: Expression): Option[Value] = expression match {
        case value: Value => Some(value)
        case _ => None
    }
	
	/**
	 * Expands symbols to their mapped expressions.
	 */
	def expand(expression: Expression)(implicit cache: ResultsCache):Expression = {
        try {
            ifValue(expression).getOrElse {
                map(expression,expander(cache))
            }
        }
        catch {
            case exception:Exception => {
                Console.err.println("Could not resolve expression: "+expression.face+".\r\nCause: "+exception.getMessage)
                throw exception
            }
        }
	}
	
	/**
	 * Substitutes symbols with their evaluated values.
	 */
	def substitute(expression: Expression)(implicit cache: ResultsCache):Expression = {
        try {
            ifValue(expression).getOrElse {
                map(expression match {
                    case symbol: Symbol => getExpression(symbol) getOrElse symbol
                    case other => other
                },substitutor(cache)) match {
                    case evalAt: EvalAt => substitute(evalAt.substitute)(cache)
                    case other => other
                }
            }
        }
        catch {
            case exception: Exception => {
                Console.err.println("Could not substitute expression: "+expression.face+".\r\nCause: "+exception.getMessage)
                throw exception
            }
        }
	}
	
	/**
	 * Evaluates given expression.
	 */
	def evaluate(expression: Expression)(implicit cache: ResultsCache = new ResultsCache()):Expression = {
        def evaluateRaw: Expression = {
            val e1 = map(expression, preEvaluator(cache))
            map(e1,evaluator(cache))
        }
        try {
            expression match {
                case value: Value => value
                case symbol: Symbol => {
                    cache.get(symbol) getOrElse {
                        val result = evaluateRaw
                        cache.put(symbol, result)
                        result
                    }
                }
                case _ =>  evaluateRaw
            }
        }
        catch {
            case exc:Exception => {
                Console.err.println("Could not evaluate expression: "+expression.face+".\r\nCause: "+exc.getMessage)
                throw exc
            }
        }
	}

    /**
     * Partially evaluates given expression.
     */
    def partiallyEvaluate(expression: Expression)(implicit cache: ResultsCache = new ResultsCache()):Expression = {
        try {
            ifValue(expression).getOrElse {
                map(expression, partialEvaluator(cache))
            }
        }
        catch {
            case exc:Exception => {
                Console.err.println("Could not evaluate expression: "+expression.face+".\r\nCause: "+exc.getMessage)
                throw exc
            }
        }
    }
	
	/**
	 * Maps expressions with given transformation.
	 */
    @tailrec
	final def map(e1:Expression, t:Transformation, c:Int = 0):Expression = {
		val e2 = e1.map(t)
		if(c>=MAX_MAP_ALL_LOOP_COUNT){
		    if(e1==e2) return e1
		    else throw new IllegalStateException("Probably circular reference: "+e1)
		}else{
			if(e1.eq(e2)) {
				return e1
			}
		}
		map(e2,t,c+1)
	}
	
	/**
	 * Single-pass resolving transformation. 
	 * Expends symbols to their mapped expressions
	 */
	private def expander(cache: ResultsCache):Transformation = {
        case value: Value => value
		case s:Symbol => this.getExpression(s).getOrElse(s)
		case de:DynamicExpression => de.f(cache)
		case e => e
	}
    
	
	/**
	 * Single-pass pre-evaluating transformation.
	 * Replaces symbols with expressions 
	 */
    private def preEvaluator(cache: ResultsCache):Transformation = {
        case s:Symbol => this.getExpression(s, cache).getOrElse(s)
        case e => e
    }
	
	/**
	 * Single-pass evaluating transformation. 
	 * Evaluates expressions.
	 */
	private def evaluator(cache: ResultsCache):Transformation = {
        case value: Value => value
        case s:Symbol => this.getExpression(s, cache) match {
            case Some(x) => x match {
                case v: Value => v
                case sel:Selection => map(sel,evaluator(cache))
                case _ => x.eval()
            }
            case None => s
        }
		case de:DynamicExpression => de.f(cache)
        case pinned: PinnedExpression => pinned.context.evaluate(pinned.symbol)(cache)
		case sl:SymbolLike => sl.eval()
		case e => e.eval()
	}
	
	/**
	 * Single-pass substituting transformation. 
	 * Substitutes symbols with their evaluated values.
	 */
	private def substitutor(cache: ResultsCache):Transformation = {
        case value: Value => value
		case symbol: Symbol => evaluate(symbol)(cache)
		case dynamic: DynamicExpression => dynamic.f(cache)
        case pinned: PinnedExpression => pinned.context.evaluate(pinned.symbol)(cache)
        case symbolLike: SymbolLike => symbolLike.eval()
		case selection: Selection => selection.trim
		case evalAt: EvalAt => evalAt.substitute
		case other => other
	}

    /**
     * Single-pass partially evaluating transformation.
     * Partially evaluates expressions.
     */
    private def partialEvaluator(cache: ResultsCache):Transformation = {
        case value: Value => value
        case o: Operation2 => {
            o.copy(evaluate(o.l)(cache), evaluate(o.r)(cache))
        }
        case o: OperationN => {
            o.copy(o.args.map(evaluate(_)(cache)): _*)
        }
        case caseTest: CaseTest => {
            caseTest.map(evaluator(cache))
        }
        case o:Assert => {
            o.copyAssert(evaluate(o.left)(cache),evaluate(o.right)(cache))
        }
        case other => other
    }
	
	def evaluateWithAndReturnCopy(er:Context, cache: ResultsCache):Context = {
	    MapContext(listMappings.map(x => (x._1,er.evaluate(x._2)(cache))):_*)
	}
	
	def evaluateAndMap(symbols:Symbol*)(cache: ResultsCache):Map[Symbol,Expression] = {
	    symbols.zip(symbols.map(s => evaluate(s)(cache))).toMap
	}
}
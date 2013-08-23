package org.encalmo.calculation

import org.encalmo.expression._

/** 
 * Expression resolver trait
 */
trait ExpressionResolver {
    
    val MAX_MAP_ALL_LOOP_COUNT:Int = 256
  
	/**
	 * Should return expression mapped to that symbol or None
	 */
	def getExpression(s:Symbol, cache: ResultsCache):Option[Expression] = {
        cache.get(s) orElse getRawExpression(s)
    }
	
	/**
	 * Should return unresolved expression mapped to that symbol or None
	 */
	def getRawExpression(s:Symbol):Option[Expression]
	
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
	 * Replaces symbols with raw expressions
	 */
	def resolve(expression: Expression)(implicit cache: ResultsCache):Expression = {
        try {
            map(expression,resolver(cache))
        }
        catch {
            case exception:Exception => {
                Console.println("Could not resolve expression: "+expression+".\r\nCause: "+exception.getMessage)
                throw exception
            }
        }
	}
	
	/**
	 * Replaces symbols with their evaluated values.
	 */
	def substitute(expression: Expression)(implicit cache: ResultsCache):Expression = {
        try {
            map(expression match {
                case symbol: Symbol => getRawExpression(symbol) getOrElse symbol
                case other => other
            },substitutor(cache)) match {
                case evalAt: EvalAt => substitute(evalAt.substitute)(cache)
                case other => other
            }
        }
        catch {
            case exception: Exception => {
                Console.println("Could not substitute expression: "+expression+".\r\nCause: "+exception.getMessage)
                throw exception
            }
        }
	}
	
	/**
	 * Evaluates given expression with substituted symbols.
	 */
	def evaluate(expression: Expression)(implicit cache: ResultsCache = new ResultsCache()):Expression = {
        def evaluateRaw: Expression = {
            val e1 = map(expression, preevaluator(cache))
            map(e1,evaluator(cache))
        }
        try {
            expression match {
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
                Console.println("Could not evaluate expression: "+expression+".\r\nCause: "+exc.getMessage)
                throw exc
            }
        }
	}
	
	/**
	 * Maps expressions with given transformation.
	 */
	def map(e1:Expression, t:Transformation, c:Int = 0):Expression = {
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
	 * Replaces symbols with raw expressions
	 */
	private def resolver(cache: ResultsCache):Transformation = {
		case s:Symbol => this.getRawExpression(s).getOrElse(s)
		case de:DynamicExpression => de.f(cache)
		case e => e
	}
    
	
	/**
	 * Single-pass pre-evaluating transformation.
	 * Replaces symbols with expressions 
	 */
    private def preevaluator(cache: ResultsCache):Transformation = {
        case s:Symbol => this.getExpression(s, cache).getOrElse(s)
        case e => e
    }
	
	/**
	 * Single-pass evaluating transformation. 
	 * Evaluates expressions with substituted symbols.
	 */
	private def evaluator(cache: ResultsCache):Transformation = {
        case s:Symbol => this.getExpression(s, cache) match {
            case Some(x) => x match {
                case v: Value => v
                case sel:Selection => map(sel,evaluator(cache))
                case _ => x.eval()
            }
            case None => s
        }
		case de:DynamicExpression => de.f(cache)
		case sl:SymbolLike => sl.eval()
		case e => e.eval()
	}
	
	/**
	 * Single-pass substituting transformation. 
	 * Replaces symbols with their evaluated values
	 */
	private def substitutor(cache: ResultsCache):Transformation = {
		case symbol: Symbol => evaluate(symbol)(cache)
		case dynamic: DynamicExpression => dynamic.f(cache)
		case symbolLike: SymbolLike => symbolLike.eval()
		case selection: Selection => selection.trim
		case evalAt: EvalAt => evalAt.substitute
		case other => other
	}
	
	def evaluateWithAndReturnCopy(er:ExpressionResolver, cache: ResultsCache):ExpressionResolver = {
	    Context(listMappings.map(x => (x._1,er.evaluate(x._2)(cache))):_*)
	}
	
	def evaluateAndMap(symbols:Symbol*)(cache: ResultsCache):Map[Symbol,Expression] = {
	    symbols.zip(symbols.map(s => evaluate(s)(cache))).toMap
	}
}
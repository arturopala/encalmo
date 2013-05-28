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
	def getExpression(s:Symbol):Option[Expression]
	
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
	def resolve(e:Expression):Expression = {
		e match {
			case s:Symbol => {
				getRawExpression(s) match {
					case Some(x) => map(x,resolver)
					case None => s
				}
			}
			case _ => map(e,resolver)
		}
	}
	
	/**
	 * Replaces symbols with their evaluated values.
	 */
	def substitute(e:Expression):Expression = {
		(e match {
			case s:Symbol => {
				getRawExpression(s) match {
					case Some(x) => map(x,substitutor)
					case None => s
				}
			}
			case _ => map(e,substitutor)
		}) match {
		    case ev:EvalAt => substitute(ev.substitute)
		    case x => x
		}
	}
	
	/**
	 * Evaluates given expression with substituted symbols.
	 */
	def evaluate(e:Expression):Expression = {
	    val e1 = e.mapAll(preevaluator)
		map(e1,evaluator)
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
	private val resolver:Transformation = {
		case s:Symbol => this.getRawExpression(s).getOrElse(s)
		case de:DynamicExpression => de.f()
		case e => e
	}
    
	
	/**
	 * Single-pass pre-evaluating transformation.
	 * Replaces symbols with expressions 
	 */
    private val preevaluator:Transformation = {
        case s:Symbol => this.getExpression(s).getOrElse(s)
        case e => e
    }
	
	/**
	 * Single-pass evaluating transformation. 
	 * Evaluates expressions with substituted symbols.
	 */
	private val evaluator:Transformation = {
		case s:Symbol => this.getExpression(s) match {
			case Some(x) => x.eval 
			case None => s
		}
		case de:DynamicExpression => de.f()
		case sl:SymbolLike => sl.eval
		case e => e.eval
	}
	
	/**
	 * Single-pass substituting transformation. 
	 * Replaces symbols with their evaluated values
	 */
	private val substitutor:Transformation = {
		case s:Symbol => this.getExpression(s) match {
			case Some(x) => x.eval
			case None => s
		}
		case de:DynamicExpression => de.f()
		case sl:SymbolLike => sl.eval
		case sel:Selection => sel.trim
		case ev:EvalAt => ev.substitute
		case e => e
	}
	
	def evaluateWithAndReturnCopy(er:ExpressionResolver):ExpressionResolver = {
	    Context(listMappings.map(x => (x._1,er.evaluate(x._2))):_*)
	}
	
	def evaluateAndMap(symbols:Symbol*):Map[Symbol,Expression] = {
	    symbols.zip(symbols.map(s => evaluate(s))).toMap
	}
	
	def evaluateToNumbersMapAndDo(symbols:Symbol*)(doIfTrue:Map[Symbol,Number]=>Expression)(doIfFalse: =>Expression):Expression = {
	    val evaluated = symbols.map(s => evaluate(s))
	    val allnumbers = evaluated.forall(_.isInstanceOf[Number])
        if(allnumbers) doIfTrue(symbols.zip(evaluated.map[Number,Seq[Number]](_.asInstanceOf[Number])).toMap) else doIfFalse
    }
}

object ExpressionResolver {
    
}
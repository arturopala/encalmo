package org.encalmo.printer.expression

import org.encalmo.common._
import org.encalmo.printer._
import org.encalmo.expression._
import MathMLTags._

/**
 * Prints expressions as MathML
 * @author artur.opala
 */
object MathMLExpressionPrinter extends ExpressionPrinter[MathMLOutput, String] {

	override def print(e: Expression, output: MathMLOutput = new MathMLOutput): MathMLOutput = {
		val t = new MathMLExpressionPrinterTraveler(output)
		e.travel(traveler = t)
		output
	}

}

/**
 * Simple Traveler printing expression as MathML xml text
 * @author artur.opala
 */
class MathMLExpressionPrinterTraveler(output: MathMLOutput) extends Traveler[Expression] {
	
	val locale = output.locale

	def writeOpeningBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.leftBracket
		}
	}

	def writeClosingBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.rightBracket
		}
	}

	def isBracketNeeded(node: Node[Expression], o: Operation): Boolean = {
		if (node.parent != null) {
			node.parent.element match {
				case o:Quot => false
				case o:Power => false
				case o:sqrt => false
				case o:cbrt => false
				case o:root => false
				case o:abs => false
				case po: Operation => po.precedence > o.precedence && (node.position > 0 || po.precedence - o.precedence > 5)
				case _ => false
			}
		} else {
			false
		}
	}

	// Traveler interface implementation

	override def onEnter(node: Node[Expression]): Unit = {
	    if(node.parent!=null){
            node.parent.element match {
                case ct:CaseTest => {
                    output.convert(ct.operator(node.position))
                }
                case _ =>
            }
        }
	    if(node.element.printable){
		    node.element match {
	    		case s: Symbol => output.symbol(s)
	    		case sl: SymbolLike => output.symbol(sl.symbol)
	    		case n: Number => output.mn(n)
	    		case tv: TextValue => output.mtext(tv.text)
	    		case o: Operation => {
	    			writeOpeningBracketIfNeeded(node, o)
	    			o match {
	    				case o: PrefixOperation => {
	    					output.mo(o.operator,"prefix")
	    				}
	    				case o:Quot => {
	    					output.start(MFRAC)
	    					output.attr("linethickness","0.6")
	    					output.body
	    				}
	    				case o:Power => {
	    					output.startb(MSUP)
	    				}
	    				case o:sqrt => {
	    					output.startb(MSQRT)
	    				}
	    				case o:cbrt => {
	    					output.startb(MROOT)
	    				}
	    				case o:root => {
	    					output.startb(MROOT)
	    				}
	    				case hypot(l,r) => {
	    					output.startb(MSQRT)
	    				}
	    				case o:abs => {
	    					output.mo("|","infix","thinmathspace","thinmathspace")
	    				}
	    				case o: NamedOperation => {
	    					o match {
	    						case _ => {
	    							output.mi(o.operator)
	    							if (!o.isInstanceOf[Operation1]) {
	    								output.leftBracket
	    							}
	    						}
	    					}
	    				}
	    				case _ => Unit
	    			}
	    		}
	            case s:Selection => {
	            	if(!s.isSingle){
		                output.start(MFENCED)
		                output.attr("open","{")
		                output.attr("close","")
		                output.attr("separators","")
		                output.body
	                }
	                output.start(MTABLE)
	                output.attr("columnalign","left")
	                output.body
	            }
	            case ct:Case => {
	                output.startb(MTR)
	            }
	            case ct:CaseTest => {
	               output.start(MTD)
	               //output.attr("rowalign","left")
	               output.body
	            }
	            case ct:CaseExpression => {
	               output.start(MTD)
	               //output.attr("rowalign","left")
	               output.body
	            }
	    		case _ => Unit
		    }
	    }else{
	    	output.startb(MTEXT)
	    	output.append(".")
	    	output.end(MTEXT)
	    }
	}

	override def onBeforeChildEnter(node: Node[Expression], position: Int, child: Expression): Unit = {
		child match {
			case o: Operation => {
				node.element match {
					case o:hypot => {
						output.startb(MSUP)
						if(!child.isInstanceOf[sqrt]
	                        && !child.isInstanceOf[cbrt]
						    && !child.isInstanceOf[root]){
							output.leftBracket
						}
					}
					case o:Power => {
						if(position==0
							&& !child.isInstanceOf[sqrt]
	                        && !child.isInstanceOf[cbrt]
						    && !child.isInstanceOf[root]){
							output.leftBracket
						}
						output.startb(MROW)
					}
					case o: Operation => {
						output.startb(MROW)
					}
					case _ => Unit
				}
			}
			case _ => {
				node.element match {
					case o:hypot => {
						output.startb(MSUP)
						output.startb(MROW)
					}
					case _ => Unit
				}
			}
		}
	}

	override def onBetweenChildren(node: Node[Expression], leftChild: Expression, rightChild: Expression): Unit = {
		node.element match {
			case o:Quot => Unit
			case o:Power => Unit
			case o:cbrt => Unit
			case o:root => Unit
			case o:hypot => {
				output.mo("+","infix","thickmathspace","thickmathspace")
			}
			case o:MultipleInfixOperation => {
				output.mo(o.operator,"infix","thickmathspace","thickmathspace")
			}
			case o: InfixOperation => {
				output.mo(o.operator,"infix","thickmathspace","thickmathspace")
			}
			case o: Operation2 => {
				output.separator
			}
			case o: OperationN => {
				output.mo("|","infix","thickmathspace","thickmathspace")
			}
            case ct:Case => {
                output.startb(MTD)
                output.convert("if")
                output.end(MTD)
            }
			case _ => Unit
		}
	}

	override def onAfterChildExit(node: Node[Expression], position: Int, child: Expression): Unit = {
		child match {
			case o: Operation => {
				node.element match {
					case o:hypot => {
						if(!child.isInstanceOf[sqrt]
	                        && !child.isInstanceOf[cbrt]
						    && !child.isInstanceOf[root]){
							output.rightBracket
						}
						output.startb(MN)
						output.append("2")
						output.end(MN)
						output.end(MSUP)
					}
					case o:Power => {
						output.end(MROW)
						if(position==0 
							&& !child.isInstanceOf[sqrt]
	                        && !child.isInstanceOf[cbrt]
						    && !child.isInstanceOf[root]){
							output.rightBracket
						}
					}  
					case o:cbrt => {
						output.end(MROW)
						output.startb(MROW)
						output.startb(MN)
						output.append("3")
						output.end(MN)
						output.end(MROW)
					}
					case o: Operation => {
						output.end(MROW)
					}
					case _ => Unit
				}
			}
			case _ => {
				node.element match {
					case o:hypot => {
						output.end(MROW)
						output.startb(MN)
						output.append("2")
						output.end(MN)
						output.end(MSUP)
					}
					case _ => Unit
				}
			}
		}
	}

	override def onExit(node: Node[Expression]) = {
		if(node.element.printable){
			node.element match {
			case o: Operation => {
				o match {
					case o: PostfixOperation => {
						output.mo(o.operator,"postfix")
					}
					case o:Quot => {
						output.end(MFRAC)
					}
					case o:Power => {
						output.end(MSUP)
					}
					case o:sqrt => {
						output.end(MSQRT)
					}
					case o:cbrt => {
						output.end(MROOT)
					}
					case o:root => {
						output.end(MROOT)
					}
					case hypot(l,r) => {
						output.end(MSQRT)
					}
    				case o:abs => {
    					output.mo("|","infix","thinmathspace","thinmathspace")
    				}
					case o: NamedOperation => {
						o match {
							case _ => {
								if (!o.isInstanceOf[Operation1]) {
									output.rightBracket
								}
							}
						}
					}
					case _ => Unit
				}
				writeClosingBracketIfNeeded(node, o)
			}
	        case s:Selection => {
	            output.end(MTABLE)
	            if(!s.isSingle) output.end(MFENCED)
	        }
	        case ct:Case => {
	            output.end(MTR)
	        }
	        case ct:CaseTest => {
	           output.end(MTD)
	        }
	        case ct:CaseExpression => {
	           output.end(MTD)
	        }
			case _ => Unit
			}
		}
	}

}
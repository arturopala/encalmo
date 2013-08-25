package org.encalmo.printer.expression

import org.encalmo.common._
import org.encalmo.printer._
import org.encalmo.expression._
import MathMLTags._
import org.encalmo.calculation.{ResultsCache, EvalAt}

/**
 * Prints expressions as MathML
 * @author artur.opala
 */
object MathMLExpressionPrinter extends ExpressionPrinter[MathMLOutput, String] {

	override def print(e: Expression, output: MathMLOutput = new MathMLOutput): MathMLOutput = {
		val t = new MathMLExpressionPrinterTraveler(output)
		e.visit(visitor = t)
		output
	}

}

/**
 * Simple Traveler printing expression as MathML xml text
 * @author artur.opala
 */
class MathMLExpressionPrinterTraveler(output: MathMLOutput) extends TreeVisitor[Expression] {
	
	val locale = output.locale

	def writeOpeningBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.leftBracket()
		}
	}

	def writeClosingBracketIfNeeded(node: Node[Expression], o: Operation): Unit = {
		if (isBracketNeeded(node, o)) {
			output.rightBracket()
		}
	}

	def isBracketNeeded(node: Node[Expression], o: Operation): Boolean = {
		if (node.parent != null) {
			node.parent.element match {
                case t:Transparent => isBracketNeeded(node.parent,o)
				case o:Quot => false
				case o:Power => false
				case o:sqrt => false
				case o:cbrt => false
				case o:root => false
				case o:abs => false
				case o:min => false
				case o:max => false
				case o:hypot => false
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
		        case tr: Transparent => Unit
	    		case sl: SymbolLike => {
	    		    if(node.parent!=null){
                        node.parent.element match {
                            case ev:EvalAt =>  output.symbol(sl.symbol,false,false)
                            case _ =>  output.symbol(sl.symbol)
                        }
                    }else{
                        output.symbol(sl.symbol)
                    }
	    		}
	    		case n: Number => {
	    		    n.unit match {
                        case u if u eq EmptyUnitOfValue => output.mn(n)
	    		        case _ => {
	    		            output.startb(MROW)
	    		            output.mn(n)
	    		            n.unit match { 
	    		                case SI.deg => n.unit.visit(visitor = this)
	    		                case _ => {
	    		                    output.start(MROW)
	    		                    output.attr("mathsize","85%")
                                    output.attr("class","unit")
                                    output.body()
                                    output.mtext(ENTITY_THIN_SPACE)
                                    n.unit.visit(visitor = this)
                                    output.end(MROW)
	    		                }
	    		            }
                            output.end(MROW)
	    		        }
	    		    }
	    		}
	    		case tv: TextValue => output.mtext(tv.text)
	    		case o: Operation => {
	    			writeOpeningBracketIfNeeded(node, o)
	    			o match {
	    				case o: PrefixOperation => {
	    					output.mo(o.operator,PREFIX)
	    				}
	    				case o:Quot => { o match {
	    				        case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) => output.startb(MROW)
	    				        case _ => {
                                    output.start(MFRAC)
                                    output.attr("linethickness","0.6")
                                    output.body()
	    				        }
	    				    }
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
	    					output.mo("|",INFIX,THINMATHSPACE,THINMATHSPACE)
	    				}
	    				case o:round => {
	    				    
	    				}
	    				case o: NamedOperation => {
	    					o match {
	    						case _ => {
	    							output.mo(o.operator,PREFIX)
	    							if (!o.isInstanceOf[Operation1]) {
	    								output.leftBracket()
	    							}else{
	    							    output.append(MathMLTags.ENTITY_THIN_SPACE)
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
		                output.body()
	                }
	                output.start(MTABLE)
	                output.attr("columnalign","left")
	                output.body()
	            }
	            case ct:Case => {
	                output.startb(MTR)
	            }
	            case ct:CaseTest => {
	               output.start(MTD)
	               //output.attr("rowalign","left")
	               output.body()
	            }
	            case ct:CaseExpression => {
	               output.start(MTD)
	               //output.attr("rowalign","left")
	               output.body()
	            }
	            case su:SimpleUnitOfValue => {
	               output.unit(su)
	            }
                case cu:ComplexUnitOfValue => {
                    cu.dimension match {
                        case 1 => 
                        case _ => {
                            output.startb(MSUP)
                        }
                    }
                   output.startb(MROW)
                }
	    		case _ => Unit
		    }
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
							output.leftBracket()
						}
					}
					case o:Power => {
						if(position==0
							&& !child.isInstanceOf[sqrt]
	                        && !child.isInstanceOf[cbrt]
						    && !child.isInstanceOf[root]){
							output.leftBracket()
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
						output.leftBracket()
					}
                    case Power(Number(r,u),any) if u!=EmptyUnitOfValue && position==0 => {
                        output.leftBracket()
                    }
                    case p:Power if position==1 => {
                        output.start(MROW)
                        output.attr("mathsize","80%")
                        output.attr("class","pwri")
                        output.body()
                    }
					case _ => Unit
				}
			}
		}
	}

	override def onBetweenChildren(node: Node[Expression], leftChild: Expression, rightChild: Expression): Unit = {
		node.element match {
			case o:Quot => o match {
                case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) => output.mo("/")
                case _ => Unit
            }
			case o:Power => Unit
			case o:cbrt => Unit
			case o:root => Unit
			case o:hypot => {
				output.mo("+",INFIX,THICKMATHSPACE,THICKMATHSPACE)
			}
			case o:Prod => {
                (leftChild,rightChild) match {
                    case (n:Number,s:Symbol) if n.unit == EmptyUnitOfValue => output.append(MathMLTags.ENTITY_THIN_SPACE)
                    case _ => output.mo(o.operator,INFIX,THINMATHSPACE,THINMATHSPACE)
                }
			}
			case o:MultipleInfixOperation => {
			    output.mo(o.operator,INFIX,THICKMATHSPACE,THICKMATHSPACE)
			}
			case o: InfixOperation => {
				output.mo(o.operator,INFIX,THINMATHSPACE,THINMATHSPACE)
			}
			case o: Operation2 => {
				output.separator()
			}
			case o: OperationN => {
				output.mo(";",INFIX,THICKMATHSPACE,THICKMATHSPACE)
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
							output.rightBracket()
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
							output.rightBracket()
						}
					}  
					case o:cbrt => {
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
					    output.rightBracket()
						output.end(MROW)
						output.start(MROW)
                        output.attr("mathsize","80%")
                        output.attr("class","pwri")
                        output.body()
						output.startb(MN)
						output.append("2")
						output.end(MN)
						output.end(MROW)
						output.end(MSUP)
					}
                    case Power(Number(r,u),any) if u!=EmptyUnitOfValue && position==0 => {
                        output.rightBracket()
                    }
                    case p:Power if position==1 => {
                        output.end(MROW)
                    }
					case _ => Unit
				}
			}
		}
	}

	override def onExit(node: Node[Expression]) = {
		if(node.element.printable){
			node.element match {
			    case tr: Transparent => Unit
    			case o: Operation => {
    				o match {
    					case o: PostfixOperation => {
    						output.mo(o.operator,"postfix")
    					}
    					case o:Quot => {o match {
                                case Quot(u1:SimpleUnitOfValue,u2:SimpleUnitOfValue) => output.end(MROW)
                                case _ => {
                                    output.end(MFRAC)
                                }
        					}
    					}
    					case o:Power => {
    						output.end(MSUP)
    					}
    					case o:sqrt => {
    						output.end(MSQRT)
    					}
    					case o:cbrt => {
    					    output.startb(MN)
                            output.append("3")
                            output.end(MN)
    						output.end(MROOT)
    					}
    					case o:root => {
    						output.end(MROOT)
    					}
    					case hypot(l,r) => {
    						output.end(MSQRT)
    					}
        				case o:abs => {
        					output.mo("|",INFIX,THINMATHSPACE,THINMATHSPACE)
        				}
                        case o:round => {
                            
                        }
    					case o: NamedOperation => {
    						o match {
    							case _ => {
    								if (!o.isInstanceOf[Operation1]) {
    									output.rightBracket()
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
                case ev:EvalAt => {
                    output.start(MROW)
                    output.attr("mathsize","85%")
                    output.attr("mathvariant","italic")
                    output.body()
                    output.startb(MTEXT)
                    output.append("(")
                    output.end(MTEXT)
                    val s = ev.er.listMappings
                    if(s.size<=5){
                        var ic = 0
                        s.foreach(
                            x => {
                               if(ic>0) {
                                   output.startb(MTEXT)
                                   output.append(",")
                                   output.append(ENTITY_THIN_SPACE)
                                   output.end(MTEXT)
                               }
                               x._1.visit(visitor = this)
                               output.startb(MTEXT)
                               output.mo("=",INFIX,THINMATHSPACE,THINMATHSPACE)
                               output.end(MTEXT)
                               ev.er.evaluate(x._2)(new ResultsCache()).visit(visitor = this)
                               ic = ic + 1
                            }
                        )
                    }
                    output.startb(MTEXT)
                    output.append(")")
                    output.end(MTEXT)
                    output.end(MROW)
                }
                case cu:ComplexUnitOfValue => {
                   output.end(MROW)
                   cu.dimension match {
                        case 1 => 
                        case _ => {
                            output.mtext(output.dimensionFormat.format(cu.dimension))
                            output.end(MSUP)
                        }
                    }
                }
    			case _ => Unit
			}
		}
	}

}
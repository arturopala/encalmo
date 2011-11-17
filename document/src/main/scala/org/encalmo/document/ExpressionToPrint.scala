package org.encalmo.document

import org.encalmo.expression._

/**
 * Expression to print holder
 * @author artur.opala
 */
case class ExpressionToPrint(
	expression:Expression,
	style:Style,
	prefix:String,
	suffix:String,
	stylesConfig:Option[StylesConfig] = None
){
    /** style's classId or standard id from styles config */
    def styleClassId:Option[String] = if(style!=null) stylesConfig.map(_.matchStyleClassId(style).getOrElse({style.classId})) else None
    
}
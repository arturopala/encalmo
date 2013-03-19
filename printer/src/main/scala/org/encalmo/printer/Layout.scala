package org.encalmo.printer

/**
 * PrintLayout class, , all dimensions in [mm]
 * @author artur.opala
 */
case class Layout(
	id:String = "default",
	format:LayoutFormat = LayoutFormat.A4,
	orientation:LayoutOrientation = LayoutOrientation.PORTRAIT,
	leftMargin:Int = 20,
	topMargin:Int = 20,
	rightMargin:Int = 20,
	bottomMargin:Int = 20,
	headerExtent:Int = 10,
	footerExtent:Int = 10,
	bodyTopMargin:Int = 12,
	bodyBottomMargin:Int = 12
){
    
    def useFormat(lf:LayoutFormat) = copy(format = lf)
    
    def useLeftMargin(d:Int):Layout = copy(leftMargin = d)
    def useRightMargin(d:Int):Layout = copy(rightMargin = d)
    def useTopMargin(d:Int):Layout = copy(topMargin = d)
    def useBottomMargin(d:Int):Layout = copy(bottomMargin = d)
    
    def portrait:Layout = copy(orientation = LayoutOrientation.PORTRAIT)
    def landscape:Layout = copy(orientation = LayoutOrientation.LANDSCAPE)
    
    def useBodyTopMargin(d:Int):Layout = copy(bodyTopMargin = d)
    def useBodyBottomMargin(d:Int):Layout = copy(bodyBottomMargin = d)
    def useHeaderExtent(d:Int):Layout = copy(headerExtent = d)
    def useFooterExtent(d:Int):Layout = copy(footerExtent = d)
    
    
}
package org.encalmo

import org.encalmo.expression.Expression

package object calculation {
    
    def dynamic(f: =>Expression) = DynamicExpression(f _)
    
}
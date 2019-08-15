set.relationship.power <-
function (relationship, the.power = NA) 
{
    is.needs.power <- is.element(multiple.generic.relationship.name(relationship), 
                                 c("Box-Cox", "Eyring"))
    
    if (any(is.needs.power)) {
        
        if (length(relationship[is.needs.power]) != length(the.power)) {
            
            cat(paste("The number of powers = ", 
                      paste(the.power, collapse = ", "), 
                      "not equal to number of power needs = ",
                      paste(relationship[is.needs.power], collapse = ", "),
                      "\n"))
            
        }
        
        needs.power.indices <- (1:length(relationship))[is.needs.power]
        
        relationship <- set.attribute(relationship, 
                                      subscript.relationship(relationship, 
                                                             needs.power.indices, allow = T), 
                                      the.power, "
                                      the.power")
    }
    
    relationship <- set.arrhenius.units(relationship)
    return(relationship)
    
}

multiple.generic.relationship.name <-
function (relationship, allow = F) 
{
    the.arrhenius.units <- attr(relationship, "the.arrhenius.units")
    the.power.vec <- attr(relationship, "the.power")
    for (i in 1:length(relationship)) {
        relationship[i] <- generic.relationship.name(relationship[i], 
            allow = allow)
    }
    attr(relationship, "the.power") <- the.power.vec
    attr(relationship, "the.arrhenius.units") <- the.arrhenius.units
    return(relationship)
}

cat.relationship <-
function (relationship.vec, relationship) 
{
    if (generic.relationship.name(relationship) == "Box-Cox") 
        the.power <- attr(relationship, "the.power")
    else the.power <- NA
    the.power.vec <- attr(relationship.vec, "the.power")
    if (length(the.power.vec) != length(relationship.vec)) {
        print(relationship.vec)
        print(the.power.vec)
        stop("Wrong length for the.power attribute in a relationship vector")
    }
    the.relationship <- c(relationship.vec, relationship)
    attr(the.relationship, "the.power") <- c(the.power.vec, the.power)
    return(the.relationship)
}

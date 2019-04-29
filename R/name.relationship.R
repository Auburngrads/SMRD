name.relationship <-
function (relationship, allow = F)
{
   if (is.null(relationship))
        return("")
    new.relationship <- relationship
    is.power <- is.element(multiple.generic.relationship.name(relationship,
        allow = allow), c("Box-Cox", "Eyring"))
    if (any(is.power)) {
        the.power <- attr(relationship, "the.power")
        if (length(the.power) != length(relationship)) {
            print(relationship)
            print(the.power)
            stop("wrong length for the.power attribute in a x vector")
        }
        new.relationship[is.power] <- paste(multiple.generic.relationship.name(relationship[is.power]),
            "(", the.power[is.power], ")", sep = "")
        attr(new.relationship, "the.power") <- the.power
    }
    return(new.relationship)
}

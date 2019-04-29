set.attribute <-
function (the.vector.relationship, focus.relationships, the.attributes, 
    the.character.attribute) 
{
    is.focus <- is.element(multiple.generic.relationship.name(the.vector.relationship), 
        focus.relationships)
    if (any(is.focus)) {
        if (length(the.attributes) != length(the.vector.relationship[is.focus])) {
            print(multiple.generic.relationship.name(the.vector.relationship))
            print(the.attributes)
            stop(paste("\n", length(the.attributes), the.character.attribute, 
                "given, but there are", length(the.vector.relationship[is.focus]), 
                "relationships"))
        }
        the.vector.attributes <- rep(NA, length(the.vector.relationship))
        the.vector.attributes[is.focus] <- the.attributes
        attr(the.vector.relationship, the.character.attribute) <- the.vector.attributes
    }
    return(the.vector.relationship)
}

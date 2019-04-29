recover.attribute <-
function (the.vector.relationship, the.character.attribute) 
{
    the.attribute <- attr(the.vector.relationship, the.character.attribute)
    if (length(the.attribute) != length(the.vector.relationship)) {
        print(the.vector.relationship)
        print(the.attribute)
        stop(paste("wrong length for", the.character.attribute, 
            "attribute in a relationship vector", paste(the.vector.relationship, 
                collapse = ",")))
    }
    names(the.attribute) <- names(the.vector.relationship)
    return(the.attribute)
}

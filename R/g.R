g <-
function (x)
{
    x.name <- deparse(substitute(x))
    if (is.factor(x) || is.character(x)) {
        return(as.factor(x))
    }
    the.relationship <- subscript.relationship(get(envir = .frame0,  "relationship.vector"), x.name)
    if (the.relationship == "class") {
        return(as.factor(x))
    }
    if (length(the.relationship) == 0) {
        warning(paste("ignored unrecognized relationship for",
            x.name))
        return(x)
    }
    f.relationship(x, the.relationship)
}

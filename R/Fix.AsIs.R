Fix.AsIs <-
function (obj)
{
    obj.name <- deparse(substitute(obj))
    has.AsIs <- check.AsIs(obj)
    the.attributes <- attributes(obj)
    fix.element <- function(subobj) {
        the.class <- oldClass(subobj)
        the.AsIs <- the.class == "AsIs"
        if (!is.null(the.class) && any(the.AsIs)) {
            the.class <- the.class[-the.AsIs]
            oldClass(subobj) <- the.class
            return(subobj)
        }
        return(subobj)
    }
    obj <- lapply(obj, fix.element)
    attributes(obj) <- the.attributes
    return(obj)
}

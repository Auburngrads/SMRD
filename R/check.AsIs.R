check.AsIs <-
function (obj) 
{
    obj.name <- deparse(substitute(obj))
    has.AsIs <- any(unique(unlist(lapply(obj, class))) == "AsIs")
    if (!is.na(has.AsIs) && has.AsIs) 
        if (map.SMRDDebugLevel() >= 4) 
            cat("\nObject", obj.name, "has AsIs")
        else {
            if (map.SMRDDebugLevel() >= 6) 
                cat("\nObject", obj.name, "has no AsIs")
            has.AsIs <- F
        }
    return(has.AsIs)
}

check.object <-
function (xobj, xobj.name, mark) 
{
    if (is.null(oldClass(xobj))) {
        warning(paste(xobj.name, " has no class, assumed to be", 
            mark))
        oldClass(xobj) <- mark
    }
    return(xobj)
}

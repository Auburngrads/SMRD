checkget <-
function (object.name) 
{
    if (!is.character(object.name)) 
        stop("object.name is not a character")
    if (!exists(object.name)) 
        stop(paste("Object", object.name, "does not exist"))
    return(get(envir = .frame0, object.name))
}

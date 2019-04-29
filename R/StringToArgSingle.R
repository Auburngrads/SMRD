StringToArgSingle <-
function (func.call, pass.arg, defstring = "") 
{
    string.arg.name <- deparse(substitute(pass.arg))
    if (is.null(pass.arg) || is.na(pass.arg) || pass.arg == defstring) 
        arg <- NULL
    else arg <- pass.arg
    func.call[[string.arg.name]] <- arg
    return(func.call)
}

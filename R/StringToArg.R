StringToArg <-
function (func.call, pass.arg, defstring = "", numeric = F) 
{
    string.arg.name <- deparse(substitute(pass.arg))
    if (is.null(pass.arg) || is.na(pass.arg) || pass.arg == defstring) 
        arg <- NULL
    else arg <- ClistToVec(pass.arg)
    if ((!is.null(arg)) && numeric) 
        arg <- as.numeric(arg)
    func.call[[string.arg.name]] <- arg
    return(func.call)
}

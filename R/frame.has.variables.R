frame.has.variables <-
function (the.frame, the.variables) 
{
    all(!is.na(match(the.variables, names(get(envir = .frame0, the.frame)))))
}

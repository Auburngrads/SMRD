substitute.sep <-
function (the.string, sep.in, sep.out) 
{
    paste(ClistToVec(the.string, sep = sep.in), collapse = sep.out, 
        sep = "")
}

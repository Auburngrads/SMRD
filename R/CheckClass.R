CheckClass <-
function (pat, obj) 
{
    regexpr(pat, oldClass(obj)) > 0
}

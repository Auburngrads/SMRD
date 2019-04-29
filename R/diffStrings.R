diffStrings <-
function (string1, string2) 
{
    ele1 <- is.element(string1, string2)
    ele2 <- is.element(string2, string1)
    cat("In string 1, but not in string2\n")
    In1Not2 <- string1[!ele1]
    print(In1Not2)
    cat("In string 2, but not in string1\n")
    In2Not1 <- string2[!ele2]
    print(In2Not1)
    invisible(list(In1Not2 = In1Not2, In2Not1 = In2Not1))
}

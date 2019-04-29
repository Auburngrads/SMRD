my.strip.blanks <-
function (str, StripChar = " ", FillChar = "") 
{
    char <- wqm.string.to.chars(str)[[1]]
    blanks <- is.element(char, StripChar)
    char[blanks] <- FillChar
    paste(char, collapse = "")
}

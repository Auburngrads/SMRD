strip.blanks.factor <-
function (str) 
{
    is.factor.str <- is.factor(str)
    if (is.factor.str) {
        factor.str <- str
        str <- attr(str, "levels")
    }
    if (length(str) == 0) 
        return("")
    for (i in 1:length(str)) {
        char.vec <- string2char(str[i])
        char.vec <- char.vec[char.vec != " "]
        str[i] <- paste(char.vec, collapse = "")
    }
    if (is.factor.str) {
        attr(factor.str, "levels") <- str
        str <- factor.str
    }
    str
}

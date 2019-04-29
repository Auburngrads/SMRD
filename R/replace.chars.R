replace.chars <-
function (string, oldchar, new.char) 
{
    single.replace.chars <- function(string, oldchar, new.char) {
        chars <- string2char(string)
        chars[chars == oldchar] <- new.char
        paste(chars, collapse = "")
    }
    for (i in 1:length(string)) {
        string[i] <- single.replace.chars(string[i], oldchar, 
            new.char)
    }
    string
}

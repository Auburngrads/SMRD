full.strip.blanks <-
function (str) 
{
    char <- wqm.string.to.chars(str)
    str <- sapply(char, function(char) {
        if (length(char) == 0 || all(char == " ")) 
            return("")
        runs <- rle(char)
        start <- 1
        end <- length(char)
        if (runs$value[1] == " ") 
            start <- start + runs$length[1]
        if (runs$value[length(runs$value)] == " ") 
            end <- end - runs$length[length(runs$length)]
        paste(char[start:end], collapse = "")
    })
    str
}

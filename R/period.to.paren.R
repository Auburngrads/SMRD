period.to.paren <-
function (string) 
{
    single.period.to.paren <- function(string) {
        chars <- string2char(string)
        periods <- chars == "."
        index.periods <- (1:length(periods))[periods]
        if (length(index.periods) == 2) {
            chars[index.periods[1]] <- "("
            chars[index.periods[2]] <- ")"
            return(paste(chars, collapse = ""))
        }
        else return(string)
        paste(chars, collapse = "")
    }
    for (i in 1:length(string)) {
        string[i] <- single.period.to.paren(string[i])
    }
    string
}

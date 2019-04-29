parse.note <-
function (the.note, line.length = (options("width")[[1]] - 9)) 
{
    if (nchar(the.note) < line.length) 
        return(the.note)
    words <- as.matrix(wqm.unpaste(the.note, " "))
    word.spacing <- diff(cumsum(apply(words, 1, nchar))%%line.length) < 
        0
    break.pos <- (1:length(word.spacing))[word.spacing]
    index.mat <- cbind(c(1, break.pos), c(break.pos - 1, length(word.spacing) + 
        1))
    paste.it <- function(indices, the.vector) {
        vector <- as.vector(the.vector)
        return(paste(unlist(the.vector[indices[1]:indices[2]]), 
            collapse = " "))
    }
    string.lines <- apply(index.mat, 1, paste.it, the.vector = words)
    return(paste(string.lines, collapse = "\n"))
}

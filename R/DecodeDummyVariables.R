DecodeDummyVariables <-
function (groupm.out) 
{
    the.relationship <- groupm.out$relationship
    is.class <- casefold(the.relationship) == "class"
    xmat <- xmat(groupm.out$the.orig.data)
    if (!is.null(xmat) && any(is.class)) {
        cat("\nSummary class dummy variable coding definition(s)\n")
        Terms <- groupm.out$terms
        the.model.matrix <- groupm.out$the.model.matrix
        term.labels <- attr(Terms, "term.labels")
        for (i in 1:length(the.relationship)) {
            if (is.class[i]) {
                cat("Class variable", term.labels[i], "\n")
                the.character.factor <- as.character(xmat[, term.labels[i]])
                the.unique.character.factor <- unique(the.character.factor)
                indices <- match(the.unique.character.factor, 
                  the.character.factor)
                model.matrix.names <- dimnames(the.model.matrix)[[2]]
                match.names <- model.matrix.names[grep(term.labels[i], 
                  model.matrix.names)]
                number.in.factor <- length(the.unique.character.factor)
                sub.model.matrix <- the.model.matrix[indices, 
                  match.names, drop = F]
                names.sub.model.matrix <- dimnames(sub.model.matrix)
                names.sub.model.matrix[[1]] <- the.unique.character.factor
                dimnames(sub.model.matrix) <- names.sub.model.matrix
                print(sub.model.matrix)
            }
        }
    }
    invisible()
}

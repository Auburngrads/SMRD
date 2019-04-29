get.default.formula <-
function (the.xmat, relationship = rep("linear", length = ncol(the.xmat)), 
    string = F) 
{
    names.x <- names(the.xmat)
    trivial <- relationship == "class" | relationship == "linear"
    beginning <- ifelse(trivial, " ", "g(")
    ending <- ifelse(trivial, " ", ")")
    the.exp.var <- paste(beginning, names.x, ending, collapse = "+", 
        sep = "")
    the.formula <- paste("Location ~ ", the.exp.var)
    if (!string) 
        the.formula <- as.formula(the.formula)
    return(the.formula)
}

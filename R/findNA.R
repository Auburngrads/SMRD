findNA <-
function (the.frame) 
{
    any.is.na <- function(x) {
        any(is.na(x))
    }
    NA.in.col <- apply(the.frame, 2, any.is.na)
    if (!any(NA.in.col)) 
        return()
    is.numeric.col <- unlist(lapply(the.frame, is.numeric))
    numeric.col.numbers <- (1:length(is.numeric.col))[is.numeric.col]
    cat("NAs found in variable(s)\n")
    print(names(the.frame)[NA.in.col])
    NA.in.row <- apply(the.frame[, numeric.col.numbers], 1, any.is.na)
    which.rows <- (1:length(NA.in.row))[NA.in.row]
    cat("NAs in the following rows\n")
    if (length(which.rows) < 50) 
        print(the.frame[NA.in.row, ])
    else print(which.rows)
    return()
}

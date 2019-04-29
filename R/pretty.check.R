pretty.check <-
function (x, transformation.in,...)
{
    if (!interactive() || (exists(".Batch") && .Batch))
        return(x)
    is.missing <- function(vector) {
        vector <- (vector < -10^30) | (vector > 10^30) | (is.na(vector))
        return(vector)
    }
    repeat {
        pretty.tran <- f.relationship(x, transformation.in)
        if (any(is.missing(pretty.tran))) {
            cat("Bad pretty values; need to replace:\n")
            print(x)
            print(pretty.tran)
            cat("Enter; hit return twice when done:\n")
            x <- scan()
      } else {
            return(x)
        }
    }
    return(x)
}

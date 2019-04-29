my.solve <-
function (a, b, tol = 1e-15) 
{
    ierr <- 0
    if (!is.list(a)) 
        a <- qr(a, tol = tol)
    if (a$rank < ncol(a$qr)) {
        ierr <- 1
        warning("apparently singular matrix")
    }
    if (missing(b)) {
        b <- a$qr
        db <- dim(b)
        if (diff(db)) 
            stop("matrix inverse only for square matrices")
        b[] <- rep(c(1, rep(0, db[1])), length = prod(db))
    }
    the.answer <- qr.coef(a, b)
    attr(the.answer, "error") <- ierr
    return(the.answer)
}

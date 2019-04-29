make.read.data <-
function (y)
{
    y <- as.matrix(y)
    dimnames(y) <- list(NULL, "time")
    frame.to.ld(frame = as.data.frame(y), response.column = "time")
}

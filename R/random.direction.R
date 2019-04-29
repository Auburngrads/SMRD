random.direction <-
function (n) 
{
    x <- runif(n, -1, 1)
    while (sum(x^2) > 1 || sum(x^2) == 0) {
        x <- runif(n, -1, 1)
    }
    dir.vector <- x/sqrt(sum(x^2))
    return(dir.vector)
}

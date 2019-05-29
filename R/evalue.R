evalue <-
function (a = 0.1, b = 0.9, conf.level = 0.95) 
{
    tail <- (1 - conf.level)/2
    evaluef <- function(x, tail, a, b) {
        tmp <- ((1 - a) * b)/(a * (1 - b))
        ans <- (x * exp(Uminus((x^2))/2) * logb(tmp))/5.01325651 - 
            tail
        return(ans)
    }
    fun.bounds <- c(qnorm(1 - tail), 10 * qnorm(1 - tail))
    cross.quant <- uniroot(evaluef, interval = fun.bounds, tol = 1e-08, 
        tail = tail, a = a, b = b)
    if (!is.R()) {
        result <- (cross.quant$neg + cross.quant$pos)/2
    }
    else {
        result <- cross.quant$root
    }
    result
}

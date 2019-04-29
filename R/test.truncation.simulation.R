test.truncation.simulation <-
function (number.sim = 20) 
{
    for (i in 1:number.sim) {
        if (i <= number.sim/2) 
            dist <- "normal"
        else dist <- "lognormal"
        tmp <- groupm.mleprobplot(data.ld = simulate.truncate.ld(), 
            distribution = dist)
        fd <- tmp[[1]]$first.derivative
        if (any(abs(fd) > 0.01)) {
            cat("\n-------------------------------------------\n")
            print(fd)
            cat("\n-------------------------------------------\n")
        }
        print(tmp[[1]]$theta.hat)
    }
}

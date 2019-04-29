CondProbInterval2 <-
function (mlest.out, age, tL, tU) 
{
    theta.hat <- mlest.out$theta.hat
    result <- CondProbInterval(mu = theta.hat[1], sigma = theta.hat[2], 
        mlest.out$distribution, age, tL, tU)
    attributes(result) <- NULL
    result
}

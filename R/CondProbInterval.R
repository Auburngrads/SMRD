CondProbInterval <-
function (mu, sigma, distribution, age, tL, tU) 
{
    age.gt.tL <- age > tL
    if (any(age.gt.tL)) 
        stop(paste("age=", age[age.gt.tL], ">", "tL=", tL[age.gt.tL]))
    tL.gt.tU <- tL > tU
    if (any(tL.gt.tU)) 
        stop(paste("tL=", tL[tL.gt.tU], ">", "tU=", tU[tL.gt.tU]))
    zC <- (log(age) - mu)/sigma
    zL <- (log(tL) - mu)/sigma
    zU <- (log(tU) - mu)/sigma
    (wqmf.phibf(zU, distribution) - wqmf.phibf(zL, distribution))/wqmf.phibm(zC, 
        distribution)
}

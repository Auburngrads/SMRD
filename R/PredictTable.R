PredictTable <-
function (mlest.out, FtimeStart, FtimeEnd, warranty.time = Inf, 
    print.total = TRUE) 
{
    data.ld <- mlest.out$data.ld
    atRiskTable <- data.ld[censor.codes(data.ld) == 2, , drop = FALSE]
    age <-Response(atRiskTable)
    NumberAtRisk <- case.weights(atRiskTable)
    time.lower <- pmin(age + FtimeStart, warranty.time)
    time.upper <- pmin(age + FtimeEnd, warranty.time)
    rho <- CondProbInterval2(mlest.out, age = age, tL = time.lower, 
        tU = time.upper)
    Efail <- rho * NumberAtRisk
    results <- data.frame(age, NumberAtRisk, rho, Efail)
    names(results) <- c("Age", "AtRisk", "rhohat", "Efail")
    attr(results, "Total") <- sum(Efail)
    if (print.total) 
        cat("Total=", attr(results, "Total"), "\n")
    results
}

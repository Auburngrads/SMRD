.distnum <-
function (number.distribution)
{
    dist.list <- c("Smallest Extreme Value", "Weibull", "Normal",
        "Lognormal", "Logistic", "Loglogistic", "Largest Extreme Value",
        "Frechet", "Log Generalized Gamma", "Generalized Gamma",
        "Log Gamma", "Gamma", "Log Exponential", "Exponential",
        "Log Inverse Gaussian", "Inverse Gaussian", "Log Birnbaum-Saunders",
        "Birnbaum-Saunders", "Log Gompertz-Makeham", "Gompertz-Makeham",
        "Log Generalized F", "Generalized F", "Uniform", "Log-Uniform",
        "Log Extended Generalized Gamma", "Extended Generalized Gamma",
        "Sev Generalized Threshold Scale", "dummy", "Normal Generalized Threshold Scale",
        "dummy")
    if (missing(number.distribution)) {
        the.numbers <- 1:length(dist.list)
        names(the.numbers) <- dist.list
        return(the.numbers)
    }
    if (!is.numeric(number.distribution))
        stop(paste("number.distribution must be numeric:", number.distribution))
    if (number.distribution <= 0 || number.distribution > length(dist.list))
        stop(paste("Distribution number out of range", number.distribution))
    else return(dist.list[number.distribution])
}

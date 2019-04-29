probability.confidence.interval <-
function (fhat, stderror, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100) 
{
    zvalue <- qnorm(1 - (1 - conf.level)/2)
    stderrq <- stderror/(fhat * (1 - fhat))
    lower <- plogis(qlogis(fhat) - zvalue * stderrq)
    upper <- plogis(qlogis(fhat) + zvalue * stderrq)
    return(list(lower = lower, upper = upper))
    invisible()
}

get.RMD.pv <-
function (mu.beta0, mu.beta1, sigma.beta0, sigma.beta1, rho, 
    sigma.measurement) 
{
    true.parameters <- c(mu.beta0 = mu.beta0, mu.beta1 = mu.beta1, 
        sigma.beta0 = sigma.beta0, sigma.beta1 = sigma.beta1, 
        rho = rho, sigma.measurement = sigma.measurement)
    oldClass(true.parameters) <- "RMD.pv"
    return(true.parameters)
}

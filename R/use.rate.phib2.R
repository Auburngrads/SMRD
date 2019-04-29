use.rate.phib2 <-
function (field.yresp, mu.userate, sigma.userate, mu.cycles, 
    sigma.cycles) 
{
    mu.field <- mu.cycles - mu.userate
    sigma.field <- sqrt(sigma.cycles^2 + sigma.userate^2)
    z <- (field.yresp - mu.field)/sigma.field
    return(pnorm(z))
}

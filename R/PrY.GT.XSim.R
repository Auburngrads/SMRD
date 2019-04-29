PrY.GT.XSim <-
function (xbar, sx, nx, ybar, sy, ny, nsim = 1e+06, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100) 
{
    xvalues <- rnorm(nsim, xbar, sx)
    yvalues <- rnorm(nsim, ybar, sy)
    Prob <- sum(yvalues > xvalues)/nsim
    return(Prob)
}

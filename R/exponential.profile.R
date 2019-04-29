exponential.profile <-
function (theta, thetahat, r) 
{
    return(exp(r + r * (logb(thetahat) - logb(theta)) - (r * 
        thetahat)/theta))
}

general.dist.mle2 <-
function (data.ld, distribution)
{
    distribution <- generic.distribution(distribution)
    gmle.out <- switch(distribution, logistic = , normal = ,
        sev = , loglogistic = , lognormal = , weibull = {
            ls.mle(data.ld, distribution = distribution)
        }, exponential = {
            expon.mle(data.ld)
        }, egeng = {
            egeng.mle(data.ld)
        }, gamma = {
            Gamma.mle(data.ld)
        }, igau = {
            igau.mle(data.ld)
        }, bisa = {
            bisa.mle(data.ld)
        }, sevgets = {
            gets.mle(small.interval(data.ld), distribution = "sev")
        }, normalgets = {
            gets.mle(small.interval(data.ld), distribution = "normal")
        }, {
            stop(paste("distribution not recognized:", distribution))
        })
    return(gmle.out)
}

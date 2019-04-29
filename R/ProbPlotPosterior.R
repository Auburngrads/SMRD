ProbPlotPosterior <-
function (posterior.object, post.or.prior = "post", marginal.on = "failure probability",
    marginal.on.detail.vec, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    interval.type = "two-sided",x.of.interest)
{
    func.call <- match.call()
    conditional.on.data <- single.ifelse(post.or.prior == "post",
        " | DATA", "")
    distribution <- generic.distribution(posterior.object$distribution)
    mean.of.marginal <- rep(NA, length = length(marginal.on.detail.vec))
    lower.bound <- rep(NA, length = length(marginal.on.detail.vec))
    upper.bound <- rep(NA, length = length(marginal.on.detail.vec))
    for (i in 1:length(marginal.on.detail.vec)) {
        marginal.on.detail <- marginal.on.detail.vec[i]
        marginal.sample <- marginalize.task(posterior.object,
            post.or.prior, marginal.on = marginal.on, marginal.on.detail = marginal.on.detail,
            x.of.interest = x.of.interest)
        length.post <- length(marginal.sample)
        marginal.trim.range <- range(trim.vector(marginal.sample,
            trim = 0.005))
        x.width <- (4 * (marginal.trim.range[2] - marginal.trim.range[1]))/(logb(length(marginal.sample),
            base = 2) + 1)
        density.out <- density(marginal.sample, width = x.width,
            from = marginal.trim.range[1], to = marginal.trim.range[2])
        marginal.sample <- sort(marginal.sample)
        mean.of.marginal[i] <- mean(marginal.sample)
        switch(interval.type, `two-sided` = {
            lowerq <- (1 - conf.level)/2
            point.to.pick <- ceiling(lowerq * length.post)
            lower.bound[i] <- marginal.sample[point.to.pick]
            point.to.pick <- floor((1 - lowerq) * length.post +
                0.5)
            upper.bound[i] <- marginal.sample[point.to.pick]
        }, none = {
        }, {
            stop(paste("illegal interval type", interval.type))
        })
    }
    results <- cbind(Time = marginal.on.detail.vec, Mean = mean.of.marginal,
        Lower = lower.bound, Upper = upper.bound)
    return(results)
}

marginalize.task <-
function (posterior.object, post.or.prior, marginal.on, marginal.on.detail, 
    x.of.interest = NULL, extra.explan.vars = NULL, use.rows = 1:length(posterior.object[[post.or.prior]][, 
        1])) 
{
    number.columns <- ncol(posterior.object[[post.or.prior]])
    distribution <- generic.distribution(posterior.object$distribution)
    posterior.object[[post.or.prior]] <- as.matrix(posterior.object[[post.or.prior]][use.rows, 
        ])
    if (is.logdist(distribution)) {
        posterior.object[[post.or.prior]][, 1] <- logb(posterior.object[[post.or.prior]][, 
            1])
    }
    explan.vars <- c(posterior.object$explan.vars, extra.explan.vars)
    if (!is.null(explan.vars)) {
        for (i in explan.vars) {
            the.relationship <- subscript.relationship(posterior.object$relationships, 
                i)
            posterior.object$xs[i] <- f.relationship(posterior.object$xs[i], 
                the.relationship)
            x.of.interest[i] <- f.relationship(x.of.interest[i], 
                the.relationship)
        }
        xs.plus <- c(posterior.object$xs[explan.vars], quant(posterior.object$p, 
            distribution))
        col.use <- c((explan.vars + 1), number.columns)
    }
    else {
        col.use <- c(number.columns)
        xs.plus <- c(quant(posterior.object$p, distribution))
    }
    posterior.object[[post.or.prior]][, 1] <- posterior.object[[post.or.prior]][, 
        1] - as.matrix(posterior.object[[post.or.prior]][, col.use]) %*% 
        xs.plus
    col.use <- col.use[1:(length(col.use) - 1)]
    if (!is.null(explan.vars)) {
        mu.at.x.of.interest.post <- posterior.object[[post.or.prior]][, 
            1] + as.matrix(posterior.object[[post.or.prior]][, 
            col.use]) %*% x.of.interest
    }
    else {
        mu.at.x.of.interest.post <- posterior.object[[post.or.prior]][, 
            1]
    }
    switch(marginal.on, Quantile = , quantile = {
        p.for.quantile <- marginal.on.detail
        x.of.interest.plus <- c(x.of.interest[explan.vars], quant(p.for.quantile, 
            distribution))
        the.marginal <- mu.at.x.of.interest.post + as.matrix(posterior.object[[post.or.prior]][, 
            col.use]) %*% x.of.interest.plus
        the.marginal <- exp(the.marginal)
    }, `failure probability` = , `Failure probability` = {
        if (is.logdist(distribution)) y.for.failure.prob <- logb(marginal.on.detail) else y.for.failure.prob <- marginal.on.detail
        the.marginal <- wqmf.phibf((y.for.failure.prob - mu.at.x.of.interest.post)/posterior.object[[post.or.prior]][, 
            number.columns], distribution)
    }, Parameter = , parameter = {
        switch(marginal.on.detail, spread = , sigma = , beta = {
            the.marginal <- posterior.object[[post.or.prior]][, 
                number.columns]
            if (distribution == "weibull") {
                the.marginal <- 1/the.marginal
            }
        }, mu = , eta = {
            the.marginal <- mu.at.x.of.interest.post
            if (distribution == "weibull") {
                the.marginal <- exp(the.marginal)
            }
        })
    })
    return(as.vector(the.marginal))
}

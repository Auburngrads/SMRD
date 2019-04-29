marginalize <-
function (prior.and.posterior, x.of.interest = NULL, p.for.quantile = NULL, 
    t.for.failure.prob = NULL, extra.explan.vars = NULL, use.rows = 1:length(prior.and.posterior$post[, 
        1])) 
{
    prior.and.posterior <- fix.old.sim.post(prior.and.posterior)
    use.rows.prior <- use.rows
    if (length(use.rows.prior) > nrow(prior.and.posterior$prior$prior)) 
        use.rows.prior <- 1:length(prior.and.posterior$prior$prior[, 
            1])
    prior.and.posterior <- fix.old.sim.post(prior.and.posterior)
    prior.and.posterior$prior$prior <- as.matrix(prior.and.posterior$prior$prior[use.rows.prior, 
        ])
    prior.and.posterior$post <- as.matrix(prior.and.posterior$post[use.rows, 
        ])
    prior.and.posterior$prior$prior[, 1] <- logb(prior.and.posterior$prior$prior[, 
        1])
    prior.and.posterior$post[, 1] <- logb(prior.and.posterior$post[, 
        1])
    distribution <- prior.and.posterior$distribution
    explan.vars <- c(prior.and.posterior$explan.vars, extra.explan.vars)
    if (!is.null(explan.vars)) {
        for (i in explan.vars) {
            the.relationship <- subscript.relationship(prior.and.posterior$relationships, 
                i)
            prior.and.posterior$xs[i] <- f.relationship(prior.and.posterior$xs[i], 
                the.relationship)
            x.of.interest[i] <- f.relationship(x.of.interest[i], 
                the.relationship)
        }
        xs.plus <- c(prior.and.posterior$xs[explan.vars], quant(prior.and.posterior$p, 
            distribution))
        col.use <- c((explan.vars + 1), ncol(prior.and.posterior$prior$prior))
    }
    else {
        col.use <- c(ncol(prior.and.posterior$prior$prior))
        xs.plus <- c(quant(prior.and.posterior$p, distribution))
    }
    prior.and.posterior$prior$prior[, 1] <- prior.and.posterior$prior$prior[, 
        1] - as.matrix(prior.and.posterior$prior$prior[, col.use]) %*% 
        xs.plus
    prior.and.posterior$post[, 1] <- prior.and.posterior$post[, 
        1] - as.matrix(prior.and.posterior$post[, col.use]) %*% 
        xs.plus
    if (is.null(t.for.failure.prob)) {
        x.of.interest.plus <- c(x.of.interest[explan.vars], quant(p.for.quantile, 
            distribution))
        marginal.prior <- prior.and.posterior$prior$prior[, 1] + 
            as.matrix(prior.and.posterior$prior$prior[, col.use]) %*% 
                x.of.interest.plus
        marginal.post <- prior.and.posterior$post[, 1] + as.matrix(prior.and.posterior$post[, 
            col.use]) %*% x.of.interest.plus
        marginal.prior <- exp(marginal.prior)
        marginal.post <- exp(marginal.post)
    }
    else {
        y.for.failure.prob <- logb(t.for.failure.prob)
        col.use <- col.use[1:(length(col.use) - 1)]
        if (!is.null(explan.vars)) {
            mu.at.x.of.interest.prior <- prior.and.posterior$prior$prior[, 
                1] + as.matrix(prior.and.posterior$prior$prior[, 
                col.use]) %*% x.of.interest
            mu.at.x.of.interest.post <- prior.and.posterior$post[, 
                1] + as.matrix(prior.and.posterior$post[, col.use]) %*% 
                x.of.interest
        }
        else {
            mu.at.x.of.interest.prior <- prior.and.posterior$prior$prior[, 
                1]
            mu.at.x.of.interest.post <- prior.and.posterior$post[, 
                1]
        }
        marginal.prior <- wqmf.phibf((y.for.failure.prob - mu.at.x.of.interest.prior)/prior.and.posterior$prior$prior[, 
            ncol(prior.and.posterior$prior$prior)], distribution)
        marginal.post <- wqmf.phibf((y.for.failure.prob - mu.at.x.of.interest.post)/prior.and.posterior$post[, 
            ncol(prior.and.posterior$post)], distribution)
    }
    return(list(prior = marginal.prior, post = marginal.post, 
        p.for.quantile = p.for.quantile, t.for.failure.prob = t.for.failure.prob, 
        extra.explan.vars = extra.explan.vars))
}

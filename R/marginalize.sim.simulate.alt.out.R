#' @export
marginalize.sim.simulate.alt.out <-
function (results.object, focus.quantity, focus.quantity.detail,
    x.of.interest = NULL, use.rows = 1:nrow(results.object),...)
{
    number.columns <- ncol(results.object)
    colnames <- dimnames(results.object)[[2]]
    names(colnames) <- dimnames(results.object)[[2]]
    model <- attr(results.object, "model")
    accelvar.units <- model$accelvar.units
    distribution <- generic.distribution(model$distribution)
    relationships <- model$relationships
    explan.vars <- model$explan.vars
    regr.param <- 1:(length(explan.vars) + 1)
    uber.results.object <- matrix(results.object[1:nrow(results.object),
        1:ncol(results.object), drop = FALSE], ncol = ncol(results.object),
        nrow = nrow(results.object), byrow = F)
    dimnames(uber.results.object) <- dimnames(results.object)
    sigma.param <- length(explan.vars) + 2
    if (!is.null(explan.vars) && is.onlist(focus.quantity, c("Parameter",
        "parameter"))) {
        if (is.character(x.of.interest))
            x.of.interest <- string.to.frame(x.of.interest)
        x.of.interest.info <- paste(" at", paste(format(x.of.interest),
            accelvar.units, collapse = ","))
        for (i in 1:length(explan.vars)) {
            the.relationship <- subscript.relationship(relationships,
                i)
            x.of.interest[i] <- f.relationship(as.numeric(x.of.interest[[i]]),
                the.relationship)
        }
        mu.at.x.of.interest <- uber.results.object[use.rows,
            regr.param, drop = FALSE] %*% c(1, unlist(x.of.interest))
        sigma.at.x.of.interest <- uber.results.object[use.rows,
            sigma.param]
    }
    else {
        mu.at.x.of.interest <- uber.results.object[use.rows,
            1]
        sigma.at.x.of.interest <- uber.results.object[use.rows,
            2]
    }
    parameter.type <- "real"
    switch(focus.quantity, Quantile = , quantile = {
        p.for.quantile <- focus.quantity.detail
        the.marginal <- mu.at.x.of.interest + quant(p.for.quantile,
            distribution) * sigma.at.x.of.interest
        if (is.logdist(distribution)) {
            the.marginal <- exp(the.marginal)
            parameter.type <- "positive"
        }
        label <- paste("t_", p.for.quantile, x.of.interest.info,
            sep = "")
    }, `failure probability` = , `Failure probability` = {
        if (is.logdist(distribution)) y.for.failure.prob <- logb(focus.quantity.detail) else y.for.failure.prob <- focus.quantity.detail
        the.marginal <- wqmf.phibf((y.for.failure.prob - mu.at.x.of.interest)/sigma.at.x.of.interest,
            distribution)
        label <- paste("F(", format(focus.quantity.detail), ")",
            x.of.interest.info, sep = "")
        parameter.type <- "probability"
    }, Parameter = , parameter = {
        the.marginal <- uber.results.object[, focus.quantity.detail]
        parameter.name <- colnames[focus.quantity.detail]
        if (parameter.name == "sigma") parameter.type <- "positive"
        if (distribution == "weibull") {
            if (parameter.name == "sigma") {
                the.marginal <- 1/the.marginal
                parameter.name <- "beta"
            }
            if (parameter.name == "mu") {
                the.marginal <- exp(the.marginal)
                parameter.name <- "eta"
                parameter.type <- "positive"
            }
        }
        label <- parameter.name
    })
    the.marginal <- as.vector(the.marginal)
    attr(the.marginal, "parameter.type") <- parameter.type
    attr(the.marginal, "label") <- label
    attr(the.marginal, "axis") <- "linear"
    oldClass(the.marginal) <- "marginal.sample"
    return(the.marginal)
}

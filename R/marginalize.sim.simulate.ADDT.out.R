#' @export
marginalize.sim.simulate.ADDT.out <-
function (results.object, focus.quantity, focus.quantity.detail,
    FailLevel, x.of.interest = NULL, use.rows = 1:nrow(results.object),...)
{
    number.columns <- ncol(results.object)
    colnames <- dimnames(results.object)[[2]]
    names(colnames) <- dimnames(results.object)[[2]]
    model <- attr(results.object, "model")
    plan.values <- attr(results.object, "plan.values")
    accelvar.units <- plan.values$accelvar.units
    distribution <- generic.distribution(model$distribution)
    relationships <- model$transformation.x
    explan.vars <- "accel.var"
    regr.param <- 1:(length(model$orig.param.names) - 1)
    sigma.param <- length(model$orig.param.names)
    if (!is.null(explan.vars) && is.onlist(focus.quantity, c("Parameter",
        "parameter"))) {
        x.of.interest <- string.to.frame(x.of.interest)
        x.of.interest.info <- paste(" at", paste(format(x.of.interest),
            accelvar.units, collapse = ","))
        FailLevelDef <- paste(" for a failure def of", FailLevel,
            get.response.units(plan.values))
    }
    parameter.type <- "real"
    uber.results.object <- matrix(results.object[1:nrow(results.object),
        1:ncol(results.object), drop = FALSE], ncol = ncol(results.object),
        nrow = nrow(results.object), byrow = F)
    switch(focus.quantity, Quantile = , quantile = {
        p.for.quantile <- focus.quantity.detail
        the.marginal <- apply(uber.results.object[, 1:length(model$orig.param.names),
            drop = F], 1, fx.ADDT.life.quantile, p = p.for.quantile,
            distribution = model$distribution, FailLevel = FailLevel,
            xuse = x.of.interest, transformation.response = model$transformation.response,
            transformation.x = model$transformation.x, transformation.time = model$transformation.time)
        label <- paste("t_", p.for.quantile, x.of.interest.info,
            FailLevelDef, sep = "")
        if (is.logdist(distribution)) parameter.type <- "positive" else parameter.type <- "real"
    }, `failure probability` = , `Failure probability` = {
        y.for.failure.prob <- focus.quantity.detail
        the.marginal <- apply(uber.results.object[, 1:length(model$orig.param.names),
            drop = F], 1, fx.ADDT.life.failure.probability, time.vec = y.for.failure.prob,
            distribution = model$distribution, FailLevel = FailLevel,
            xuse = x.of.interest, transformation.response = model$transformation.response,
            transformation.x = model$transformation.x, transformation.time = model$transformation.time)
        label <- paste("F(", format(focus.quantity.detail), ")",
            x.of.interest.info, FailLevelDef, sep = "")
        parameter.type <- "probability"
    }, Parameter = , parameter = {
        the.marginal <- uber.results.object[, focus.quantity.detail]
        parameter.name <- colnames[focus.quantity.detail]
        if (parameter.name == "sigma") parameter.type <- "positive"
        if (distribution == "weibull") {
            if (parameter.name == "sigma") {
                the.marginal <- 1/the.marginal
                parameter.name <- "beta"
                parameter.type <- "positive"
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

#' Title
#'
#' @param posterior.object 
#' @param task 
#' @param marginal.on.pos 
#' @param marginal.on.sigma 
#' @param type.position 
#' @param newdata 
#' @param include.likelihood 
#' @param post.or.prior 
#' @param xlim 
#' @param ylim 
#' @param xlab 
#' @param ylab 
#' @param axes.range.default.post 
#' @param factor 
#' @param size 
#' @param number.points.plot 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using a non-informative quantile 
#' # and a noninformative sigma
#' 
#' prior.spec1 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "loguniform",
#'                        qlower = 100,
#'                        qupper = 5000,
#'                        sigma.dist =  "lognormal",
#'                        sigma.lower = 0.2,
#'                        sigma.upper =  0.5,
#'                        distribution = "Weibull")
#'                        
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using a noninformative quantile 
#' # and an informative sigma
#' 
#' prior.spec2 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "loguniform",
#'                        qlower = 1000,
#'                        qupper = 1400,
#'                        sigma.dist = "lognormal",
#'                        sigma.lower = 1.5,
#'                        sigma.upper = 2.5, 
#'                        distribution  = "Weibull")
#'                        
#' # Specify prior distribution characteristics for the 
#' # \code{bearingcage} data using an informative quantile 
#' # and an informative sigma
#' 
#' prior.spec3 <-
#'   specify.simple.prior(p = .01,
#'                        qdist = "lognormal",
#'                        qlower = 1000,
#'                        qupper = 1400,
#'                        sigma.dist = "lognormal",
#'                        sigma.lower = 1.5,
#'                        sigma.upper = 2.5,
#'                        distribution  = "Weibull")
#'  
#' # Create the prior distributions                      
#' prior3.bcage <- 
#'   make.prior(spec = prior.spec3, 
#'              number.in.prior = 3000)
#' 
#' 
#' prior.and.post3.bcage <-
#'   get.big.posterior(prior.spec3,
#'                     BearingCage.ld)
#' 
#' prior.and.post3.bcage$post[1:10,] 
#' 
#' prior.and.post3.bcage <- 
#'   make.small.posterior.object(prior.and.post3.bcage)
#'   
#' summarize.posterior.or.prior(prior.and.post3.bcage,
#'                              post.or.prior = "post",
#'                              task = "Marginals only",
#'                              marginal.on.sigma = T,
#'                              marginal.on.pos = F,
#'                              type.position = "Parameter",
#'                              newdata = "mu",
#'                              include.likelihood = T)
#' }
summarize.posterior.or.prior <-
function (posterior.object, task, marginal.on.pos, marginal.on.sigma, 
    type.position, newdata, include.likelihood, post.or.prior, 
    xlim = c(NA, NA), ylim = c(NA, NA), xlab = parameter.contour.axes.labels.out$xlab, 
    ylab = parameter.contour.axes.labels.out$ylab, axes.range.default.post = T, 
    factor = 4.5, size = GetSMRDDefault("SMRD.DefaultGridPoints"), 
    number.points.plot = 500) 
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    func.call <- match.call()
    switch(post.or.prior, Posterior = , post = {
        post.or.prior <- "post"
    }, Prior = , prior = {
        post.or.prior <- "prior"
    }, {
        warning(paste("bad post.or.prior--setting to post", 
            post.or.prior))
        post.or.prior <- "post"
    })
    distribution <- generic.distribution(posterior.object$distribution)
    parameter.contour.axes.labels.out <- parameter.contour.axes.labels(posterior.object$specifications.for.prior, 
        type.position)
    switch(task, `Marginal only` = , `Marginals only` = {
        if (marginal.on.sigma) {
            plot.function.task.marginals(posterior.object, post.or.prior, 
                marginal.on = "Parameter", marginal.on.detail = "spread", 
                xlim = xlim, ylim = ylim)
        }
        if (marginal.on.pos) {
            plot.function.task.marginals(posterior.object, post.or.prior, 
                marginal.on = type.position, marginal.on.detail = newdata, 
                xlim = xlim, ylim = ylim)
        }
    }, `Joint only` = {
        plot.joint.prior(posterior.object, post.or.prior, type.position = type.position, 
            newdata = newdata, xlim = xlim, ylim = ylim, 
            include.likelihood = include.likelihood, axes.range.default.post = axes.range.default.post, 
            size = size, factor = factor, number.plot = number.points.plot)
    }, Both = , `Joint w/Marginal` = , `Joint and Marginal` = {
        plot.joint.prior.or.post.marginals(posterior.object, 
            post.or.prior, type.position = type.position, newdata = newdata, 
            xlim = xlim, ylim = ylim, include.likelihood = include.likelihood, 
            axes.range.default.post = axes.range.default.post, 
            number.plot = number.points.plot)
    }, {
        stop(paste("Unrecognized task", task))
    })
    invisible()
}

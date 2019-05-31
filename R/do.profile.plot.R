#' Title
#'
#' @param gmle.out 
#' @param parameter 
#' @param original.parameter 
#' @param quantile 
#' @param failure.probability.at 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' BearingCage.ld <- frame.to.ld(bearingcage,
#'                               response.column = 1, 
#'                               censor.column = 2, 
#'                               case.weight.column = 3,
#'                               time.units = "Hours")
#' 
#' summary(BearingCage.ld)
#' 
#' BearingCage.weibull.gmle.out <- ls.mle(BearingCage.ld,
#'                                        distribution = "Weibull")
#' 
#' names(BearingCage.weibull.gmle.out)
#' 
#' do.profile.plot(BearingCage.weibull.gmle.out,
#'                 parameter = "log(scale)")
#' 
#' do.profile.plot(BearingCage.weibull.gmle.out,
#'                 original.parameter = "scale")
#' 
#' do.profile.plot(BearingCage.weibull.gmle.out,
#'                 quantile = 0.1)
#' 
#' do.profile.plot(BearingCage.weibull.gmle.out,
#'                 failure.probability = 5000)
#' 
#' }
do.profile.plot <-
function (gmle.out, parameter, original.parameter, quantile, 
    failure.probability.at) 
{
    all.of.them <- c(!missing(parameter), !missing(original.parameter), 
        !missing(quantile), !missing(failure.probability.at))
    the.index <- (1:length(all.of.them))[all.of.them]
    if (length(the.index) != 1) 
        stop("must specify exactly one of the arguments: quantile, original.parameters, or failure.probability.at")
    switch(the.index, {
        FUN <- function(x, parameter) {
            x[parameter]
        }
        parameter <- parameter
        xlab <- gmle.out$model$t.param.names[parameter]
        the.profile <- Fr.profile(FUN, gmle.out, xlab = xlab, 
            parameter = parameter)
    }, {
        FUN <- function(theta.hat, original.parameter, pass.gmle.out) {
            f.origparam <- pass.gmle.out$model$f.origparam
            f.origparam(theta.hat, pass.gmle.out$model)[original.parameter]
        }
        xlab <- gmle.out$model$orig.param.names[original.parameter]
        the.profile <- Fr.profile(FUN, gmle.out, xlab = xlab, 
            original.parameter = original.parameter, pass.gmle.out = gmle.out)
    }, {
        FUN <- function(x, quantile, pass.gmle.out) {
        }
        xlab <- "tmp"
        the.thing <- quantile
    }, {
        FUN <- function(theta, failure.probability.at, pass.gmle.out) {
            pgenmax.rev <- function(theta, tvec, gmle.out) {
                f.origparam <- gmle.out$model$f.origparam
                distribution <- gmle.out$model$distribution
                pgenmax(tvec, distribution, f.origparam(theta, 
                  gmle.out$model))
            }
            pgenmax.rev(theta = theta, tvec = failure.probability.at, 
                gmle.out = pass.gmle.out)
        }
        xlab <- paste("F(", format(failure.probability.at), ")")
        the.profile <- Fr.profile(FUN, gmle.out, xlab = xlab, 
            original.parameter = original.parameter, pass.gmle.out = gmle.out)
    })
    profile.plot(the.profile)
}

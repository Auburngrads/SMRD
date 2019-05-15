#' Title
#'
#' @param results.object 
#' @param task 
#' @param marginal.on.fq1 
#' @param marginal.on.fq2 
#' @param focus.quantity1 
#' @param focus.quantity.detail1 
#' @param x.of.interest1 
#' @param focus.quantity2 
#' @param focus.quantity.detail2 
#' @param x.of.interest2 
#' @param plot.type 
#' @param number.points.plot 
#' @param xlim 
#' @param ylim 
#' @param my.title 
#' @param xlab 
#' @param ylab 
#' @param filter.bad 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' NelsonInsulation.Weibull.altpv  <- 
#'   get.alt.plan.values.from.slope.and.point(distribution = "Weibull",
#'                                            slope= c(-12.28,-1.296),
#'                                            relationship = c("log","log"),
#'                                            accelvar.units = c("vpm","cm"),
#'                                            time.units = "Hours", 
#'                                            censor.time = 1000, probs = c(1.8e-6),
#'                                            accelvar = c(80,.266), 
#'                                            beta = 1/.6734, 
#'                                            use.conditions = c(80,.266))
#' 
#' print(NelsonInsulation.Weibull.altpv)
#' 
#' ##	define two-variable ALT test plans
#' 
#' NelsonInsulation.altplan <- 
#'   get.alt.test.plan.direct(accel.variable.levels = cbind(c(120,120,120,150,150,150,175,175,175,200,200,200),
#'                                                          c(.163,.266,.355,.163,.266,.355,.163,.266,.355,.163,.266,.355)),
#'                            number.of.units = c(11,18,11,8,14,8,8,14,8,11,18,11),
#'                            censor.times = rep(1000,12),
#'                            accelvar.names = c("Volts per mm","Thick"),
#'                            describe.string = "NelsonInsulation Factorial Plan")
#' 
#' print(NelsonInsulation.altplan)
#' print(NelsonInsulation.Weibull.altpv)
#' 
#' ##	compute the fisher and vcv matrices
#' 
#' ALT.vcv(NelsonInsulation.altplan,
#'         NelsonInsulation.Weibull.altpv)
#' 
#' ##	compute the large-sample approximate precision (R) factors
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          quantile.of.interest = c(.1,.5))
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          use.conditions = c(175,.163),
#'          quantile.of.interest = c(.1,.5))
#' 
#' evaluate(NelsonInsulation.altplan, 
#'          NelsonInsulation.Weibull.altpv,
#'          use.conditions = c(100,.1),
#'          quantile.of.interest = c(.1,.5))
#' 
#' ##	sample size needed for a given value of R
#' 
#' plot.alt.sample.size(NelsonInsulation.altplan,
#'                      NelsonInsulation.Weibull.altpv)
#' 
#' 
#' NelsonInsulation.sim.out <- ALTsim(NelsonInsulation.altplan, 
#'                                    NelsonInsulation.Weibull.altpv, number.sim = 400,
#'                                    show.detail.on = 1)
#' 
#' ALT.plot.time.v.x(NelsonInsulation.sim.out)
#' 
#' ALT.plot.time.v.x(NelsonInsulation.sim.out,
#'                   x.of.interest = c(100,.1),
#'                   xlim = c(100,200))
#' 
#' ALT.plot.FracFail.v.Time(NelsonInsulation.sim.out)
#' 
#' ALT.plot.FracFail.v.Time(NelsonInsulation.sim.out,x.of.interest = c(25,10), 
#'                          xlim = c(100,10000))
#' 
#' summarize.simultation.results(NelsonInsulation.sim.out, 
#'                               "Joint and Marginal", 
#'                               focus.quantity1 = "quantile",
#'                               focus.quantity.detail1 = 0.1,
#'                               x.of.interest1 = "100;.1",
#'                               focus.quantity2 = "parameter",
#'                               focus.quantity.detail2 = 3,
#'                               x.of.interest2 = NA,
#'                               plot.type = "density")
#' 
#' 
#' }
summarize.simultation.results <-
function (results.object, task, marginal.on.fq1 = T, marginal.on.fq2 = T, 
    focus.quantity1, focus.quantity.detail1, x.of.interest1 = NA, 
    focus.quantity2, focus.quantity.detail2, x.of.interest2 = NA, 
    plot.type = "histogram", number.points.plot = 500, xlim = c(NA, 
        NA), ylim = c(NA, NA), my.title = NULL, xlab = NULL, 
    ylab = NULL, filter.bad = TRUE, ...) 
{
    old.par <- par(err = -1)
    on.exit({
        par(old.par)
    })
    if (filter.bad) {
        ierstuff <- attr(results.object, "ierstuff")
        bad.ones <- ierstuff > 0
        if (any(bad.ones)) {
            if (sum(bad.ones) == length(bad.ones)) 
                stop(paste("All simulation", length(bad.ones), 
                  "samples were bad"))
            results.object <- results.object[!bad.ones, ]
        }
    }
    if (number.points.plot <= 0) 
        number.points.plot <- 500
    switch(task, `Marginal only` = , `Marginals only` = {
        if (marginal.on.fq1) {
            plot.marginals.sim(results.object, focus.quantity = focus.quantity1, 
                focus.quantity.detail = focus.quantity.detail1, 
                x.of.interest = x.of.interest1, plot.type = plot.type, 
                xlim = xlim, xlab = xlab, my.title = my.title, 
                ...)
        }
        if (marginal.on.fq2) {
            plot.marginals.sim(results.object, focus.quantity = focus.quantity2, 
                focus.quantity.detail = focus.quantity.detail2, 
                x.of.interest = x.of.interest2, plot.type = plot.type, 
                xlim = ylim, xlab = ylab, my.title = my.title, 
                ...)
        }
    }, `Joint only` = {
        plot.joint.sim(results.object, focus.quantity1 = focus.quantity1, 
            focus.quantity.detail1 = focus.quantity.detail1, 
            x.of.interest1 = x.of.interest1, focus.quantity2 = focus.quantity2, 
            focus.quantity.detail2 = focus.quantity.detail2, 
            x.of.interest2 = x.of.interest2, number.points.plot = number.points.plot, 
            xlim = xlim, ylim = ylim, my.title = my.title, 
            xlab = xlab, ylab = ylab, ...)
    }, Both = , `Joint w/Marginal` = , `Joint and Marginal` = {
        plot.joint.and.marginals.sim(results.object, focus.quantity1 = focus.quantity1, 
            focus.quantity.detail1 = focus.quantity.detail1, 
            x.of.interest1 = x.of.interest1, focus.quantity2 = focus.quantity2, 
            focus.quantity.detail2 = focus.quantity.detail2, 
            x.of.interest2 = x.of.interest2, number.points.plot = number.points.plot, 
            xlim = xlim, ylim = ylim, my.title = my.title, 
            xlab = xlab, ylab = ylab, plot.type = plot.type, 
            ...)
    }, Neither = {
    }, {
        stop(paste("Unrecognized task", task))
    })
    invisible()
}

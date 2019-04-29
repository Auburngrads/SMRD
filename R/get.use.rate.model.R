get.use.rate.model <-
function (use.rate.parameters, lab.parameters, distributions, 
    lab.time.units = "Test cycle", field.time.units = "Time", 
    correlation.model = NULL) 
{
    if (!is.null(correlation.model)) {
        switch(correlation.model, UR.UR.independent = {
            rho <- 0
        }, Wear.Ratio.independent = {
            rho <- use.rate.parameters["Wear.sigma.use.rate"]/use.rate.parameters["Crack.sigma.use.rate"]
            if (rho > 1) {
                rho <- 0.999
                warning("rho>1 set to .999")
            }
        }, Crack.Ratio.independent = {
            rho <- use.rate.parameters["Crack.sigma.use.rate"]/use.rate.parameters["Wear.sigma.use.rate"]
            if (rho > 1) {
                rho <- 0.999
                warning("rho>1 set to .999")
            }
        }, FixedRho = {
            rho <- attr(correlation.model, "fixed.rho")
        }, {
            stop("Unrecognized correlation.model")
        })
    }
    else {
        stop("NULL rho")
    }
    failure.modes = c("Crack", "Wear")
    names(failure.modes) <- failure.modes
    rlist <- list(use.rate.parameters = use.rate.parameters, 
        lab.parameters = lab.parameters, distributions = distributions, 
        lab.time.units = lab.time.units, field.time.units = field.time.units, 
        rho = rho, failure.modes = failure.modes, correlation.model = correlation.model)
    class(rlist) <- "use.rate.model"
    return(rlist)
}

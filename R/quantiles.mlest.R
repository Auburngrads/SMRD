#' @export
quantiles.mlest <-
function (x, 
          prob.vec = as.numeric(ClistToVec(GetSMRDDefault("SMRD.DefaultQuantileList"))),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100, 
          shape = NULL,
          digits = GetSMRDDefault("SMRD.DigitsPrinted"), 
          to = 0.999,
          add.title = NULL, 
          printem = T,...)
{
    data.ld <- x$data.ld
    distribution <- distnum(numdist(x$distribution))
    if (any(prob.vec >= 1) || any(prob.vec <= 0)) {
        warning("probabilities for quantiles outside 0-1 removed from list")
        prob.vec <- prob.vec[prob.vec > 0 & prob.vec < 1]
    }
    length.to <- min(length(prob.vec[prob.vec <= to + 1e-10]) +
        1, length(prob.vec))
    prob.vec <- prob.vec[1:length.to]
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    conf.char <- percent.conf.level(conf.level)
    conf.int.title <- paste("\nPointwise Approximate ", conf.char,
        " Confidence Intervals", sep = "")
    my.title <- paste("Using ", get.data.title(data.ld), add.title,
        "\n", "Parametric ML Quantile Estimates ", conf.int.title,
        "\n", sep = "")
    the.list <- get.parametric.quantiles(x, prob.vec = prob.vec,
        conf.level = conf.level, shape = shape)
    the.table <- cbind(p = the.list$prob.vec, Quanhat = the.list$tquanhat,
        Stderror = the.list$stderror, Lower = the.list$lower,
        Upper = the.list$upper)

    colnames(the.table) <- c("p", "Quanhat", "Std.Err.", paste(conf.char, "Lower"),
                             paste(conf.char, "Upper"))
    attr(the.table, "title") <- my.title
    attr(the.table, "x") <- x
    oldClass(the.table) <- c("quantiles.out", "estimates.out", "matrix")
    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}

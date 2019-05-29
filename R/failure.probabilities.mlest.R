#' @export
failure.probabilities.mlest <-
function (x,
          time.vec = NULL,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          shape = NULL,
          digits = GetSMRDDefault("SMRD.DigitsPrinted"),
          add.title = NULL,
          printem = T,
          time.range = NULL,
          survival = F,
          number = 10,...)
{
    data.ld <- x$data.ld
    distribution <- distnum(numdist(x$distribution))

    old <- options(digits = digits)
    on.exit(options(old))

    conf.char <- paste(floor(conf.level * 1000 + 0.01)/10,
                       "%",
                       sep = "")

    conf.int.title <- paste("\nPointwise Approximate ",
                            conf.char,
                            " Confidence Intervals",
                            sep = "")

    if (!is.null(time.range)) {
        time.vec <- get.time.vector(data.ld,
                                    distribution = distribution,
                                    number = number,
                                    time.range = time.range)
    }
    if (is.null(time.vec) || any(time.vec == Inf)) {
        time.vec <- get.time.vector(data.ld,
                                    distribution = distribution,
                                    number = number)
    }
    log.of.data <- is.logdist(distribution)

the.list <- get.parametric.bands.zhat(x,
                                      xlim = time.vec,
                                      conf.level = conf.level,
                                      shape = shape,
                                      need.list = F)
    if (survival) {
        the.table <- cbind(Times = the.list$times,
                           Shat = 1 - the.list$fhat,
                           Stderror = the.list$stderror,
                           Lower = 1 - the.list$upper,
                           Upper = 1 - the.list$lower)
        EstName <- "Shat"
        TableContents <- "Parametric ML Survival Probability Estimates "
  } else {
        the.table <- cbind(Times = the.list$times,
                           Fhat = the.list$fhat,
                           Stderror = the.list$stderror,
                           Lower = the.list$lower,
                           Upper = the.list$upper)
        EstName <- "Fhat"
        TableContents <- "Parametric ML CDF Estimates "
    }
    my.title <- paste("Using ",
                      get.data.title(data.ld),
                      add.title,
                      "\n",
                      TableContents,
                      conf.int.title,
                      "\n",
                      sep = "")

    if (is.null(the.table)) return(NULL)

    if (ncol(the.table) == 5) {
        colnames(the.table) <- c(get.time.units(data.ld),
                                 EstName,
                                 "Std.Err.",
                                 paste(conf.char, "Lower"),
                                 paste(conf.char, "Upper"))
    } else {

    `if`(ncol(the.table) == 2,
        colnames(the.table) <- c(get.time.units(data.ld), EstName),
        colnames(the.table) <- c(get.time.units(data.ld), EstName, "Std.Err."))
    }

    attr(the.table, "title") <- my.title
    attr(the.table, "x") <- x
    oldClass(the.table) <- c("failure.probabilities.out",
                             "estimates.out",
                             "matrix")

    MysetOldClass(attr(the.table, "class"))
    return(the.table)
}

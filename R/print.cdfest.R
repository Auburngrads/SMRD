#' @export
print.cdfest <-
function (x, 
          #conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          #band.type = "pointwise", 
          #a.limit = 0.001, 
          #b.limit = 0.999,
          #digits = GetSMRDDefault("SMRD.DigitsPrinted"), 
          #quote = T,
          #prefix = "",
          ...)
{
    # old <- options(digits = digits)
    # on.exit(options(old))
    # if (band.type == "none") band.type <- "pointwise"
    # time.units <- get.time.units(x$data.ld)
    # line1<-paste("Nonparametric estimates from", get.data.title(x$data.ld), sep = " ")
    # x$p[x$p < 0] <- 0
    # the.bands <- list()
    # conf.char <- percent.conf.level(conf.level)
    # extra.names <- NULL
    # if (!is.null(x$sd)) {
    #     
    #     extra.names <- c("SE_Fhat", 
    #                      paste(conf.char, "Lower"),
    #                      paste(conf.char, "Upper"))
    #     
    #     the.bands <- get.npbands(x, 
    #                              band.type, 
    #                              conf.level = conf.level,
    #                              how.show.interval = "step.fun", 
    #                              a.limit = a.limit,
    #                              b.limit = b.limit)
    #     
    #     line2 <- paste(" with approximate ", paste(100 * conf.level,
    #         "%", sep = "")," ", band.type, " confidence intervals.", sep = "")
    # }
    # the.text  <- paste(line1, line2, sep = "")
    # the.table <- cbind(x$p, 
    #                    x$q, 
    #                    x$prob, 
    #                    x$sd, 
    #                    the.bands$lower,
    #                    the.bands$upper)
    # 
    # colnames(the.table) <- c(paste(time.units, "-lower", sep = ""), 
    #                          paste(time.units, "-upper", sep = ""), "Fhat", extra.names)
    cat(paste0(noquote(x$text),"\n\n"))
    
    
    print(x$table)

    #invisible(prlist)
}

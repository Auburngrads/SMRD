warranty.sequence <-
function (warranty, miles.limit = 36, time.limit = 36, max.number = NULL,
    xlim = c(1, 1000), ylim = c(1, 1000), six.page = T,
    sigma.factor = 2.5, postscript.out = T)
{
    period.list <- as.character(unique(warranty[, "Period"]))
    periodx.list <- as.character(unique(warranty[, "Periodx"]))
    if (is.null(max.number))
        max.number <- length(period.list)
    warranty.gmle.out.list <- list()
    browser()
    for (i in 1:max.number) {
        warranty.sub <- warranty[warranty$Period == period.list[i],
            ]
        warranty.ld <- frame.to.ld(warranty.sub, response.column = "K.Miles",
            censor.column = "Censor", case.weight.column = "Weight",
            x.columns = c("Months", "Period"), data.title = paste("Labor Code",
                as.character(warranty$Lcode[1]), "Cars Built",
                period.list[i], "to", periodx.list[i]))
        if (!good.data(warranty.ld)) {
            print(paste("Skipping", get.data.title(warranty.ld),
                "because too few failures"))
            warranty.gmle.out.list[[period.list[i]]] <- list(theta.hat = c(0,
                0), origparam = rep(0, 5), dummy = T)
            next
        }
        now <- proc.time()[3]
        browser()
        warranty.gmle.out.list[[period.list[i]]] <- warranty.mle(warranty.ld)
        time.used <- proc.time()[3] - now
        warranty.gmle.out.list[[period.list[i]]]$Lcode <- as.character(warranty$Lcode[1])
        warranty.gmle.out.list[[period.list[i]]]$Lcode <- as.character(warranty$Lcode[1])
    }
    assign(envir = .frame0,  inherits = TRUE,"tmp.warranty.gmle.out.list", warranty.gmle.out.list)
    the.array <- array(unlist(lapply(warranty.gmle.out.list,
        range.bvn.gmle.out, sigma.factor = sigma.factor)), dim = c(2,
        2, 6))
    the.array.xlim <- c(apply(the.array, c(2), min)[1], apply(the.array,
        c(2), max)[1])
    the.array.ylim <- c(apply(the.array, c(2), min)[2], apply(the.array,
        c(2), max)[2])
    xrna <- is.na(xlim)
    if (any(xrna))
        xlim[xrna] <- the.array.xlim[xrna]
    yrna <- is.na(ylim)
    if (any(yrna))
        ylim[yrna] <- the.array.ylim[yrna]
    plot.warranty.sequence(warranty.gmle.out.list, miles.limit = miles.limit,
        time.limit = time.limit, max.number = max.number, xlim = xlim,
        ylim = ylim, six.page = six.page, sigma.factor = sigma.factor,
        postscript.out = postscript.out)
    oldClass(warranty.gmle.out.list) <- "multiple.gmle.out"
    invisible(warranty.gmle.out.list)
}

#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param plot.dist 
#' @param xlab 
#' @param xlim 
#' @param ylim 
#' @param time.range 
#' @param conf.level 
#' @param interactive 
#' @param original.par 
#' @param mle.quantiles 
#' @param my.title 
#' @param type 
#' @param cex 
#' @param cex.points 
#' @param cexlab 
#' @param sub.title 
#' @param grids 
#' @param linear.axes 
#' @param slope.axis 
#' @param print.table 
#' @param title.option 
#' @param ylab 
#' @param trunc.correct 
#' @param add 
#' @param plot.censored.ticks 
#' @param pch 
#' @param lty 
#' @param lwd 
#' @param length.time.vec 
#' @param compare.dists 
#' @param col.ci 
#' @param lwd.ci 
#' @param xxx.mle.out 
#' @param ciMethod 
#' @param extrapolate.ci 
#' @param band.type 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AlloyC.ld <- frame.to.ld(alloyc,
#'                          response.column = c(1,2),
#'                          censor.column = 3,
#'                          case.weight.column = 4,
#'                          data.title = "Alloy C",
#'                          time.units = "ksi")
#' 
#' AlloyC.sevgets.gmle.out <- gets.mle(AlloyC.ld, distribution = "sev")
#' 
#' AlloyC.norgets.gmle.out <- gets.mle(AlloyC.ld, distribution = "normal")
#' 
#' basic.gmleprobplot(AlloyC.ld, distribution = "sevgets",
#'                    xxx.mle.out = AlloyC.sevgets.gmle.out,
#'                    my.title = "",
#'                    cexlab = 1.5,
#'                    plot.dist = "sev")
#' 
#' AlloyC.gets.nor.gmle.out <- gets.mle(AlloyC.ld, distribution = "normal")
#' 
#' basic.gmleprobplot(AlloyC.ld,
#'                    distribution = "normalgets",
#'                    xxx.mle.out = AlloyC.norgets.gmle.out, 
#'                    my.title = "", 
#'                    cexlab = 1.5,
#'                    plot.dist = "sev")
#' 
#' }
basic.gmleprobplot <-
function (data.ld, 
          distribution, 
          plot.dist, 
          xlab = get.time.units(data.ld),
          xlim = c(NA, NA),
          ylim = c(NA, NA),
          time.range = c(NA,NA),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          interactive = T, 
          original.par = F, 
          mle.quantiles = T, 
          my.title = NULL,
          type = "y",
          cex = 1, 
          cex.points = 1.2, 
          cexlab = 1, 
          sub.title = "",
          grids = F, 
          linear.axes = F, 
          slope.axis = F, 
          print.table = F,
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          trunc.correct = T, 
          add = F, 
          plot.censored.ticks = F, 
          pch = 16,
          lty = c(3, 4, 5, 6, 7), 
          lwd = c(2, 2, 2, 2, 2), 
          length.time.vec = 100,
          compare.dists = NULL, 
          col.ci = 4, 
          lwd.ci = 2, 
          xxx.mle.out = NULL,
          ciMethod = "normal.approx", 
          extrapolate.ci = F, 
          band.type = "p",...)
{
    
    if (missing(plot.dist) || is.null(plot.dist)) {
        
        `if`(is.onlist(generic.distribution(distribution), c("exponential","sev", "normal", "logistic", "lognormal", "weibull","loglogistic")),
             plot.dist <- distribution,
             plot.dist <- "Weibull")
        
    }
    
    `if`(band.type != "n",
         conf.int.title <- paste("and Pointwise", percent.conf.level(conf.level),"Confidence Intervals"),
         conf.int.title <- "")
    
    if (is.null(my.title)) {
        
        my.title <- paste(get.data.title(data.ld), 
                          "with", distribution,
                          "ML Estimate ", "\n", conf.int.title)
        
    }
    cdfest.out <- cdfest(data.ld)
    cdpoints.out <- cdpoints(cdfest.out)
    
    if(!is.null(xxx.mle.out) && tolower(xxx.mle.out$model$distribution) == tolower(distribution)) {
       
        gmle.out <- xxx.mle.out
        
    } else {
        
        if (!is.null(xxx.mle.out)) warning("Mismatch between distribution and supplied results")
        gmle.out <- general.dist.mle2(data.ld, distribution = distribution)
        
    }
    
    return.gmle.out <- gmle.out
    theta <- gmle.out$origparam
    ybandrange <- NULL
    xtvna <- is.na(time.range)
    if (any(xtvna)) time.range[xtvna] <- range(cdpoints.out$yplot)[xtvna]
    
    `if`(is.logdist(distribution),
         time.vec <- seq(time.range[1], time.range[2], length = length.time.vec),
         time.vec <- logseq(time.range[1], time.range[2], length = length.time.vec))
    
    probs <- pgenmax(time.vec, distribution = distribution, theta = theta)
    
    fhat.and.se <- f.gendeltamethod(gmle.out$origparamvcv, 
                                    gmle.out$origparam,
                                    pgenmax, 
                                    distribution = distribution, 
                                    tvec = time.vec)
    fhat <- fhat.and.se$vec
    goodones <- fhat > 0 & fhat < 1
    se <- fhat.and.se$se
    fhat <- fhat[goodones]
    se <- se[goodones]
    
    if (band.type != "n") {
        switch(ciMethod, normal.approx = {
            fhat.and.se <- f.gendeltamethod(gmle.out$origparamvcv,
                gmle.out$origparam, pgenmax, distribution = distribution,
                tvec = time.vec)
            fhat <- fhat.and.se$vec
            goodones <- fhat > 0 & fhat < 1
            se <- fhat.and.se$se
            fhat <- fhat[goodones]
            se <- se[goodones]
            zvalue <- qnorm(1 - (1 - conf.level)/2)
            wvec <- exp((zvalue * se)/(fhat * (1 - fhat)))
            lower <- fhat/(fhat + (1 - fhat) * wvec)
            upper <- fhat/(fhat + (1 - fhat)/wvec)
        }, lr.approx = {
            pgenmax.rev <- function(theta, tvec, gmle.out) {
                F.origparam <- gmle.out$model$f.origparam
                distribution <- gmle.out$model$distribution
                pgenmax(tvec, distribution, F.origparam(theta,
                  gmle.out$model))
            }
            lrci.stuff <- Fr.conf(pgenmax.rev, 
                                  fcn.arg2 = time.vec,
                                  gmle.out = gmle.out, 
                                  extrapolate = extrapolate.ci,
                                  conf.level = conf.level, 
                                  ptwise.if.simult = T)
            
            lower <- lrci.stuff$lower
            upper <- lrci.stuff$upper
        })
        ybandrange <- range(strip.na(lower), strip.na(upper))
    }
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(time.range, cdpoints.out$yplot)[xrna]
    
    yrna <- is.na(ylim) 
    if (any(yrna)) ylim[yrna] <- range(cdpoints.out$pplot, ybandrange)[yrna]
    
    `if`(!add,
         log.of.data <- probplot.setup(distribution = plot.dist,
                                       xlim = range(xlim),
                                       ylim = ylim, 
                                       my.title = my.title,
                                       sub.title = sub.title, 
                                       cex = cex, cexlab = cexlab,
                                       grids = grids, 
                                       linear.axes = linear.axes, 
                                       title.option = title.option,
                                       slope.axis = slope.axis, 
                                       ylab = ylab, xlab = xlab,
                                       title.line.adj = -2, ...),
         log.of.data <- get.prob.scales(plot.dist, 
                                        shape = NULL,
                                        prob.range = ylim)$logger)
    
    if (type != "n")
        points.default(pp.data(cdpoints.out$yplot, log.of.data),
                       quant(cdpoints.out$pplot, plot.dist), 
                       cex = cex.points,
                       pch = pch)
    
    if (mle.quantiles) {
        
        lines(pp.data(time.vec, log.of.data), 
              quant(probs, plot.dist),
              lwd = 2, 
              lty = 1)
        
        if (band.type != "n") {
            
            lines(pp.data(time.vec[goodones], log.of.data), 
                  quant(mono.lower(lower), plot.dist), 
                  lwd = lwd.ci, 
                  lty = 3, 
                  col = col.ci)
            lines(pp.data(time.vec[goodones], log.of.data), 
                  quant(mono.upper(upper), plot.dist), 
                  lwd = lwd.ci, 
                  lty = 3, 
                  col = col.ci)
            
            bands.list <- list(time.vec = time.vec, 
                               probs = probs,
                               lower = mono.lower(lower), 
                               upper = mono.upper(upper))
            
       } else {
           
            bands.list <- list(time.vec = time.vec, probs = probs)
            
       }
        
    }
    if (!is.null(compare.dists) && (is.vector(compare.dists) || is.list(compare.dists))) {
        
        compare.distribution.vec <- rep("", length(compare.dists))
        
        for (idist in 1:length(compare.dists)) {
            
            compare.dists.now <- compare.dists[[idist]]
            
            if (is.character(compare.dists.now)) {
                
                gmle.out <- general.dist.mle2(data.ld, 
                                              distribution = compare.dists.now)
                thetacomp <- gmle.out$origparam
                compare.distribution.vec[idist] <- compare.dists.now
                comprobs <- pgenmax(time.vec, 
                                    theta = thetacomp,
                                    distribution = compare.dists.now)
                
            } else {
                
                thetacomp <- compare.dists.now$origparam
                compare.distribution.vec[idist] <- compare.dists.now$model$distribution
                comprobs <- pgenmax(time.vec, 
                                    distribution = compare.distribution.vec[idist],
                                    theta = thetacomp)
                
            }
            
            lines(pp.data(time.vec, log.of.data), 
                  quant(comprobs, plot.dist), 
                  lwd = lwd[idist + 1], 
                  lty = lty[idist + 1], 
                  col = idist + 1)
            
        }
        
        if (!is.null(compare.dists)) {
            
            legend(x.loc(0.01), 
                   y.loc(0.98), 
                   c(distribution, compare.distribution.vec), 
                   lty = c(1, lty[1:length(compare.dists)]),
                   lwd = c(2, lwd[1:length(compare.dists)]),
                   col = 1:(length(compare.dists) + 1),
                   y.intersp = 0.675)
            
        }
        
    }
    
    attr(return.gmle.out, "bands.list") <- bands.list
    return(return.gmle.out)
}

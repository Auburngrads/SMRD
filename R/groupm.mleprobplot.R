#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param formula 
#' @param group.var 
#' @param xlab 
#' @param ylab 
#' @param conf.level 
#' @param xlim 
#' @param ylim 
#' @param relationship 
#' @param power 
#' @param dump 
#' @param mle.intervals 
#' @param cex 
#' @param grids 
#' @param slope.axis 
#' @param linear.axes 
#' @param lty 
#' @param plot.censored.ticks 
#' @param time.range 
#' @param shape 
#' @param ci.list 
#' @param col.ci 
#' @param printem 
#' @param trunc.correct 
#' @param include.interaction 
#' @param new.data 
#' @param plotem 
#' @param do.legend 
#' @param stresses.limit 
#' @param number.points 
#' @param plotted 
#' @param from.six.plot 
#' @param debug1 
#' @param theta.start 
#' @param parameter.fixed 
#' @param compute.subsets 
#' @param check.level 
#' @param title.line.adj 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ICDevice2.ld <- frame.to.ld(icdevice2, 
#'                             response.column = c(1,2),
#'                             censor.column = 3,
#'                             case.weight.column = 4,
#'                             x.column = 5,  
#'                             data.title = "New Technology Device ALT", 
#'                             xlabel = "Degrees C", 
#'                             time.units = "Hours")
#' 
#' groupi.mleprobplot(ICDevice2.ld, 
#'                    distribution = "Lognormal")
#' 
#' ICDevice02.groupm.lognor <- groupm.mleprobplot(ICDevice2.ld, 
#'                                                distribution = "Lognormal", 
#'                                                relationship = "Arrhenius", 
#'                                                ci.list = 6)
#' }
groupm.mleprobplot <-
function (data.ld,
          distribution,
          formula = NULL,
          group.var = 1:ncol(xmat(data.ld)),
          xlab = get.time.units(data.ld),
          ylab = GetSMRDDefault("SMRD.LabelOnYaxis"),
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          xlim = c(NA,NA),
          ylim = c(NA,NA),
          relationship = NULL,
          power = NULL,
          dump = 0,
          mle.intervals = F,
          cex = 1,
          grids = F,
          slope.axis = F,
          linear.axes = F,
          lty = NULL,
          plot.censored.ticks = F,
          time.range = c(NA,NA),
          shape = NULL,
          ci.list = NULL,
          col.ci = 4,
          printem = F,
          trunc.correct = T,
          include.interaction = F,
          new.data = NULL,
          plotem = T,
          do.legend = "On plot",
          stresses.limit = 18,
          number.points = 55,
          plotted = rep(T, length(stresses.plus)),
          from.six.plot = F,
          debug1 = F,
          theta.start = NULL,
          parameter.fixed = NULL,
          compute.subsets = T,
          check.level = SMRDOptions("SMRD.DataCheck"),
          title.line.adj,
          lwd = 2,
          mar = c(4.5, 5.25, 3.5, 12.1),
          bty = `if`(grids, "o","L"),...)
{

not.stripped <- function (data.d)
{
  return(attr(data.d, "not.stripped"))
}

if (missing(title.line.adj)) { title.line.adj = -3 }

the.orig.xmat      <- xmat(data.ld)
the.orig.data.ld   <- data.ld
the.xmat           <- as.data.frame(the.orig.xmat[, group.var, drop = F])
reduced.xmat.names <- dimnames(the.xmat)[[2]]
    
if (is.null(relationship)) {
  
    group.var.numeric <- unlist(lapply(the.xmat, is.numeric))
    relationship <- rep("linear", length = length(group.var))
    relationship[!group.var.numeric] <- "class"
    
}
    
relationship.sanity(the.xmat, relationship)
relationship <- set.relationship.power(relationship, power)
names(relationship) <- reduced.xmat.names
    
for (i in 1:ncol(the.xmat)) {
  
    if (generic.relationship.name(relationship[i]) == "class") {
        the.xmat[, i] <- as.factor(the.xmat[, i])
    }
  
}

dimnames(the.xmat)[[2]] <- reduced.xmat.names
assign(envir = .frame0,  
       inherits = TRUE,
       "relationship.vector", 
       relationship)
    
if (is.null(formula)) formula <- get.default.formula(the.xmat, 
                                                     relationship)
Terms <- terms(formula)

the.data.frame <- data.frame(Response = rep(1, nrow(the.xmat)),
                             the.xmat)
    
names(the.data.frame)[1] <- as.character(attr(Terms,
                                              "variables"))[2]

the.model.matrix <- model.matrix(Terms, the.data.frame)

attr(the.model.matrix, "contrast.method") <- as.character(.Options$contrasts)

stresses <- get.x.markers(data.ld, 
                           group.var = group.var,
                           do.order = F)
    
ordered.stresses <- get.x.markers(data.ld, 
                                   group.var = group.var,
                                   do.order = T)
    
`if`(length(relationship) == 1 && relationship == "class",
     stress.names <- get.x.markers(data.ld, 
                                    group.var = group.var,
                                    do.order = F),
     stress.names <- get.x.markers(data.ld, 
                                    group.var = group.var,
                                    long = T, do.order = F))
    
`if`(!is.null(not.stripped(data.ld)),
     { the.not.stripped <- not.stripped(data.ld)
       stress.order <- stress.order(data.ld) },
     { the.not.stripped <- rep(T, length(stresses))
       stress.order <- match(ordered.stresses, stresses) })
    
if (map.SMRDDebugLevel() >= 6) {
    cat("\n unordered stresses \n")
    print(stresses)
    cat("\n unordered stress names \n")
    print(stress.names)
}

stress.names <- stress.names[stress.order]
stresses <- stresses[stress.order]
    
if (map.SMRDDebugLevel() >= 6) {
    cat("\n ordered stresses \n")
    print(stresses)
    cat("\n ordered stress names \n")
    print(stress.names)
}

if (length(relationship) != length(group.var)) {
  
    stop(paste("\nLength of relationship =", 
               length(relationship),
               " must equal length of group.var =", 
               length(group.var)))
  
}

if (plotem && length(stresses) > stresses.limit) {
    warning(paste("\nThere are", 
                  length(stresses), 
                  "different explanatory variable combinations.\n",
                  "A probability plot will not be made.\n"))
  
    plotem <- F
    compute.subsets <- F

    if (from.six.plot) {
        return.list <- "dummy"
        attr(return.list, "plotem") <- plotem
        return(return.list)
    }
}

xmat(data.ld) <- model.matrix.to.matrix(the.model.matrix)
attr(data.ld, "the.relationships") <- relationship
    
mlest.out <- mlest(data.ld, 
                   distribution, 
                   explan.vars = seq(1:ncol(xmat(data.ld))),
                   theta.start = theta.start, 
                   parameter.fixed = parameter.fixed,
                   kprint = dump, 
                   send.col.ones = T, 
                   intercept = F,
                   embedded = T,...)

xmat(data.ld) <- the.xmat
mlest.out$relationship <- relationship
mlest.out$power <- power

xnames.matrix1 <- as.matrix(reduced.xmat.names)
xnames.matrix2 <- apply(xnames.matrix1, 1, regexpr, attr(Terms, "term.labels"))
xnames.matrix3 <- apply(as.matrix(xnames.matrix2), 2, max) > 0
attr(Terms, "xnames") <- reduced.xmat.names[xnames.matrix3]
mlest.out$terms <- Terms

mlest.out$the.model.matrix <- the.model.matrix
mlest.out$group.var <- 1:length(group.var)
mlest.out$stress.names <- stress.names
mlest.out$stresses <- stresses

attr(data.ld, "xlabel") <- reduced.xmat.names
mlest.out$data.ld <- data.ld
mlest.out$title <- paste(get.data.title(data.ld), 
                         "\n", 
                         paste(reduced.xmat.names,
                               name.relationship(relationship, 
                                                 allow = T), 
                               sep = "", collapse = ", "), 
                         paste(", Dist:", distribution, sep = ""))

mlest.out$the.orig.data.ld <- the.orig.data.ld
return.list <- list(groupm.out = mlest.out)

assign(envir = .frame0, inherits = !TRUE,"test.groupm.out", return.list)
bands.list <- list()
nonparametric.list <- list()
ylim.data <- NULL
xlim.quant <- NULL
stresses.plus <- c(stress.names)

if (!is.null(new.data)) {
  
    new.data <- as.data.frame(new.data)
    x.names <- colnames(the.orig.xmat)
    names(new.data) <- x.names[group.var]
    stresses.plus <- unique(c(stress.names, 
                              apply(new.data, 
                                    1, 
                                    paste, 
                                    reduced.xmat.names, 
                                    collapse = ";")))
    
}

plotted <- c(the.not.stripped, 
             rep(T, length(stresses.plus) - length(stress.names)))
    
if (is.null(lty)) {
      
    `if`(GetSMRDDefault("SMRD.solid.lines"),
         lty <- rep(1, length(plotted)),
         lty <- (1:(length(plotted) + 1))[-2])
  
}
    
pch          <- (1:(length(plotted) + 1))[-2]
col.fhat.vec <- (1:(length(plotted) + length(col.ci) + 1))[-col.ci]
pch          <- pch[plotted]
col.fhat.vec <- col.fhat.vec[plotted]
lty          <- lty[plotted]
cdfest.out   <- cdfest(data.ld)
cdpoints.out <- cdpoints(cdfest.out)
upper.quantile.max <- NULL
    
if (compute.subsets) {
      
if (plotem) {
  
  on.exit(par(xpd = F, bty = "o", mar = c(5, 4, 4, 2) + 0.1,err = -1))
  
}
      
for (i in 1:length(stresses.plus)) {
  
data.name <- stresses.plus[i]
            
`if`(is.onlist(i, ci.list),
     conf.level.send <- conf.level,
     conf.level.send <- 0 )
          
if (map.SMRDDebugLevel() >= 6)  cat("stress = ", 
                                stresses.plus[i], 
                                "ci=", 
                                conf.level.send,
                                "\n")

if (i <= length(stresses)) {
              
  if (map.SMRDDebugLevel() >= 6) cat("stress = ", 
                                     stresses[i], 
                                     "stress name = ",
                                     stress.names[i], 
                                     "\n")
              
  data.subset.ld <- multiple.get.data.subset(data.ld,
                                             stresses[i], 
                                             columns = 1:ncol(the.xmat))
  if (map.SMRDDebugLevel() >= 4) {
    
      cat("*******Loop index:", stresses.plus[i], "\n")
      print(data.subset.ld)
  }
        
  if (is.null(data.subset.ld))  break
              
  single.xmat <- as.data.frame(xmat(data.subset.ld))
  dimnames(single.xmat)[[2]] <- reduced.xmat.names
  get.location.out <- get.single.dist(mlest.out,
                                       single.xmat[1, , drop = F])
                
  mlest.dummy <- list(distribution = distribution,
                      theta.hat = get.location.out$thetavec, 
                      vcv.matrix = get.location.out$vcv,
                      y = Response(mlest.out$data.ld), 
                      ierfit = 0,
                      iervcv = 0)
  
  if (!good.data(data.subset.ld, 
                 check.level = check.level,
                 number.needed = 1)) {
    
    if (plotem) message(paste("Skipping", 
                              paste(stress.names[i],collapse = " "), 
                              "in probability plot because too few failures\n"))
} else {
  
    cdfest.out <- cdfest(data.subset.ld)
    if (length(cdfest.out$q) > 0) {
      
        cdpoints.out <- cdpoints(cdfest.out)
        trunc.correct <- (!is.null(cdfest.out$left.trun.cond)   ||
                          !is.null(cdfest.out$right.trun.cond)) && 
                          trunc.correct
        
    if (trunc.correct) {
      
        mlest.subset.out <- mlest(data.subset.ld, distribution)
        cdpoints.out <- truncadj(cdpoints.out,
                                 mlest.dummy,
                                 debug1 = debug1)
    }
    nonparametric.list[[data.name]] <- cdpoints.out
  }
}
if (printem) {
    cat("\n\nAt conditions ", stress.names[i],":\n \n", sep = "")
    print(get.location.out$thetavec)
    cat("\n")
    print(get.location.out$vcv)
    cat("\n\n")
}
sublist <- list(stresses = stresses[i], 
                theta.hat = get.location.out$thetavec,
                vcv.matrix = get.location.out$vcv, 
                distribution = distribution,
                data.ld = data.subset.ld, 
                kodet = c(1, 2))

oldClass(sublist) <- "mlest"

return.list[[stress.names[i]]] <- sublist

upper.quantile     <- 0.99 * max(cdpoints.out$pplot + 0.01)
upper.quantile.max <- max(upper.quantile, upper.quantile.max)
the.quantiles      <- quantiles.mlest(mlest.dummy,
                                      printem = F, 
                                      to = upper.quantile)[, "Quanhat"]

xlim.quant.now <- range(the.quantiles)
xlim.quant     <- range(xlim.quant, xlim.quant.now)
xtvna          <- is.na(time.range)

 `if`(any(!xtvna),
      { xlim.quant <- range(time.range[!xtvna], xlim.quant)
        xlim.quant.use <- xlim.quant },
      { xlim.quant.use <- xlim.quant.now })
 
bands <- get.parametric.bands.zhat(mlest.dummy,
                                   conf.level = conf.level.send, 
                                   xlim = xlim.quant.use)
} else {
  
  inow <- i - length(stresses)
  the.quantiles <- quantiles.groupm.out(mlest.out,
                                        new.data = new.data[inow, , drop = F], 
                                        printem = F,
                                        to = upper.quantile.max)
  the.quantiles <- the.quantiles[, "Quanhat"]
  the.quantiles <- the.quantiles[the.quantiles != Inf]
  
  tv.extend <- NULL
  xtvna <- is.na(time.range)
  
  if (any(!xtvna)) {
      xlim.quant <- range(time.range[!xtvna], xlim.quant)
      tv.extend <- range(time.range[!xtvna])
  }
  
  `if`(length(the.quantiles) > 0,
       { xlim.quant <- range(xlim.quant, the.quantiles)
         tv.range <- c(the.quantiles, tv.extend) },
       { tv.range <- xlim.quant })
  
  fail.prob.out <- 
    failure.probabilities.groupm.out(mlest.out,
                                     new.data = new.data[inow, , drop = F], 
                                     time.vec = vec.from.range(range(tv.range),distribution, number.points = number.points),
                                     printem = F, 
                                     conf.level = conf.level.send)
  if (is.null(fail.prob.out)) {
      bands.list[[data.name]] <- "dummy"
      next
  }
  `if`(ncol(fail.prob.out) > 3,
       bands <- list(times = fail.prob.out[, 1], 
                     fhat  = fail.prob.out[, 2], 
                     lower = fail.prob.out[, 4], 
                     upper = fail.prob.out[, 5]),
       bands <- list(times = fail.prob.out[, 1], 
                     fhat  = fail.prob.out[, 2]))
            }
bands.list[[data.name]] <- bands
ylim.data <- range(ylim.data, 
                   cdpoints.out$pplot,
                   bands$fhat, 
                   bands$lower, 
                   bands$upper)

if (dump) browser()
}

if (plotem && length(nonparametric.list) == 0) {
          
    warning(paste("No estimable data sets in", get.data.title(data.ld)))
    plotem <- F
            
} else {
          
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- ylim.data[yrna]
    
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- xlim.quant[xrna]
            
}

if (plotem) {
  
log.of.data <- probplot.setup(distribution, 
                              xlim,
                              ylim, 
                              xlab = xlab, 
                              ylab = ylab,
                              grids = grids, 
                              linear.axes = linear.axes, 
                              slope.axis = slope.axis,
                              cex = cex, 
                              title.line.adj = title.line.adj,
                              mar = mar,
                              bty = bty,...)
}

bands.list.data.names <- names(bands.list)

        if (map.SMRDDebugLevel() >= 6) {
            cat("\nbands.list\n")
            print(bands.list.data.names)
            cat("\nstresses.plus\n")
            print(stresses.plus)
        }

for (i in 1:length(bands.list)) {
  
data.name <- bands.list.data.names[i]

if (plotem) {
  
    if (is.onlist(data.name, names(nonparametric.list))) {
      
        cdpoints.out <- nonparametric.list[[data.name]]
        points.default(pp.data(cdpoints.out$yplot, log.of.data), 
                       quant(cdpoints.out$pplot, distribution),
                       col = col.fhat.vec[i], 
                       pch = pch[i]%%19,
                       cex = (1.2 * GetSMRDDefault("SMRD.point.size"))/100)
    }
bands <- bands.list[[data.name]]
if (is.null(bands$times)) next
times <- bands$times

lines(pp.data(times, log.of.data), 
      pp.quant(bands$fhat, distribution, shape), 
      col = col.fhat.vec[i],
      lty = lty[i], 
      lwd = lwd)

if (!is.null(bands$lower)) {
  
    lines(pp.data(times, log.of.data), 
          pp.quant(bands$lower, distribution, shape), 
          col = col.ci, 
          lty = 3,
          lwd = lwd)
    lines(pp.data(times, log.of.data), 
          pp.quant(bands$upper, distribution, shape), 
          col = col.ci, 
          lty = 3,
          lwd = lwd)
}
}
}

f.plot.censored.ticks(data.ld, 
                      log.of.data, 
                      plot.censored.ticks)
pch.done <- -pch
pch.done[1:length(stresses)] <- -pch.done[1:length(stresses)]

if (do.legend == "On plot" && plotem) {
  
    par(xpd = T)
  
    legend(x.loc(1.05), 
           y.loc(0.99), 
           legend = parse(text = switch.units(stresses.plus, data.ld)),
           cex = 1, 
           bty = "n", 
           col = col.fhat.vec, 
           lty = lty,
           lwd = lwd,
           seg.len = 1.5,
           pch = pch.done%%19, 
           y.intersp = 1,
           adj = c(-0.1))
}
if (do.legend == "New page" && plotem) {
  
    plot(c(0, 0), 
         c(1, 1), 
         xlab = "", 
         ylab = "",
         type = "n", 
         xaxt = "n", 
         yaxt = "n")
   legend(x.loc(0.003), 
          y.loc(0.994), 
          legend = parse(text = switch.units(stresses.plus,data,ld)),
          cex = 1, 
          bty = "n", 
          col = col.fhat.vec, 
          lty = lty,
          pch = pch.done%%19, 
          y.intersp = 0.675)
}
}

`if`(length(group.var) == 1,
     oldClass(return.list) <- c("alt.fit", "groupm.out"),
     oldClass(return.list) <- c("groupm.out"))

attr(return.list, "plotem") <- plotem
invisible(return.list)

}

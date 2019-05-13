#' An S4 class to represent a lifedata object.
#'
#' @slot frame A \code{data.frame} class object
lifedata <- setClass("lifedata",
                     slots = c(frame = "data.frame"))

#' Plot the lifedata class to standard output.
#'
#' @param x A \code{lifedata} object.
#' @param distribution A character vector of distributions names
#' @param events A logical value. If \code{TRUE} an event plot is returned,
#'               otherwise an estimate of the cdf is returned.
#'
#' @return NULL. Plots to standard out.
#'
#' @name plot
#' @aliases plot plot,lifedata-method
#' @docType methods
#' @rdname plot-methods
#' @export
#'
#' @seealso \code{\link{plot}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' plot(lz.ld, events = T)
#' plot(lz.ld, distribution = 'weibull')
#' plot(lz.ld, distribution = c('weibull','lognormal'))
#' getMethod("plot", "lifedata")
#' }
setMethod("plot", 
          signature = "lifedata",
          definition = function(x,
                                distribution = NULL,
                                events = F,
                                my.title = NULL, 
                                xlab = paste(SMRD2:::get.time.units(x@frame)),
                                ylab = `if`(is.null(SMRD2:::case.weights(x@frame)), "Unit", ""),
                                title.option = SMRD2:::GetSMRDDefault("SMRD.TitleOption"), 
                                cex.labs = 1.1, 
                                cex.tic.lab = 1.1,
                                which.units.to.plot = 1:nrow(x@frame), 
                                original.par = T, 
                                print.row = T,
                                count.on.right = T, 
                                suppress.ones = T, 
                                failpch = 8, 
                                fail.cex = 0.9,
                                fail.col = 1, 
                                add = F,...){
            
if(!events) {
  
   if(is.null(distribution)) SMRD2:::cdfplot(data.ld = x,...)
  
   dist_length = length(distribution)
   
   if(dist_length == 3 | dist_length > 4) stop('Number of distributions must be 1, 2, or 4')

   if(dist_length == 4) SMRD2:::four.npprobplot(data.ld = x, distribution.list = distribution,...)
      
   if(dist_length == 2) SMRD2:::two.npprobplot(data.ld = x, distribution.list = distribution,...)

   if(dist_length == 1) SMRD2:::npprobplot(data.ld = x, distribution = distribution,...)
       
} else {

    label.factor <- 1
    label.line <- 0.75
    y <- SMRD2:::Response(x@frame)
    number.cases <- nrow(y)
    the.case.weights <- SMRD2:::case.weights(x@frame)
    if (is.null(the.case.weights)) the.case.weights <- rep(1, number.cases)
    the.censor.codes <- SMRD2:::censor.codes(x@frame)
    if (is.null(the.censor.codes))the.censor.codes <- rep(1, number.cases)
    the.censor.codes <- as.character(the.censor.codes)
    iusys <- as.character(1:number.cases)
    number.unit.to.plot <- length(which.units.to.plot)
    maxx <- 1.05 * max(y[which.units.to.plot, ])
    minx <- 0 - maxx/30
    if (is.null(my.title))my.title <- SMRD2:::get.data.title(x@frame)
    old.par <- par(mar = c(4.35, 5.1, 0.1, 4.1))
    if (original.par) on.exit(par(old.par))
    ylim <- c(0, 1)
    y.axis <- "linear"
    x.axis <- "linear"
    if (!add) {
      GetAxesRange.out <- SMRD2:::GetAxesRange("event.plot.setup",
                                       x.axis, 
                                       xlim = c(0, maxx), 
                                       xlab, 
                                       y.axis, 
                                       ylim,
                                       ylab)
      maxx <- GetAxesRange.out$xlim[2]
      minx <- 0 - maxx/30
      
      plot(c(minx, maxx), 
           ylim, 
           type = "n", 
           xlab = "", 
           ylab = "",
           yaxt = "n", 
           xaxt = "n", 
           axes = FALSE)
      
      getxax.out <- SMRD2:::linax(0, maxx)
      datax.tick.location <- as.numeric(getxax.out$ticloc)
      datax.tick.label.loc <- as.numeric(getxax.out$ticlab)
      axis(side = 1, at = datax.tick.location, labels = F,
           tck = -0.01, mgp = c(5, 2.1, 0), cex.axis = 1.1)
      xlabels <- SMRD2:::vector.power10(getxax.out$ticlab)
      if (SMRD2:::is.postsctiptok() && substring(xlabels[1], 1, 1) ==
          "~") {
        SMRD2:::mixed.mtext.vec(side = 1, at = datax.tick.label.loc,
                        texts = xlabels, adj = 0.5, cex = cex.labs/label.factor,
                        line = label.line)
        axis(side = 1, at = datax.tick.label.loc, labels = F,
             tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
      } else {
        axis(side = 1, at = datax.tick.label.loc, labels = SMRD2:::fix.exp.labels(getxax.out$ticlab),
             adj = 0.5, tck = -0.02, mgp = c(5, 1, 0), cex.axis = cex.tic.lab)
      }
      title(xlab = xlab, cex.lab = 1.1)
      mtext(side = 2, "Interval / Failure Event", cex = 1.1, line = 0.75)
      if (title.option == "full")
        title(my.title)
    }
    textx <- par("usr")[1] + 0.1
    ydelta <- 1/(number.unit.to.plot + 1)
    circle.delta <- min(ydelta, 1/20)
    if (SMRD2:::map.SMRDDebugLevel() >= 4) cat("circle.delta=", circle.delta, "\n")
    vindic <- min(ydelta/3, 1/(11 * 3))
    ypos <- (number.unit.to.plot + 1) * ydelta
    ypos.inc <- 0.25 * ydelta
    horiz.inc <- (SMRD2:::x.loc(1) - SMRD2:::x.loc(0))/50
    case.weight.dominate <- length(the.case.weights[the.case.weights ==
                                                      1])/length(the.case.weights) > 0.1
    if (number.unit.to.plot > number.cases)
      stop("Number requested for plotting greater than number of units")
    the.ypos <- rep(0, number.cases)
    the.failure.modes <- SMRD2:::failure.modes(x@frame)
    if (!is.null(the.failure.modes)) {
      mode.plot.sym <- rep("$", length(the.failure.modes))
      plot.failure.modes <- T
      character.mode <- as.character(the.failure.modes)
      the.failed <- SMRD2:::is.onlist(casefold(character.mode), SMRD2:::ClistToVec(GetSMRDDefault("SMRD.RcName")))
      if (length(unique(substring(character.mode[the.failed],
                                  1, 1))) == length(unique(character.mode[the.failed]))) {
        mode.plot.sym[the.failed] <- substring(character.mode[the.failed],
                                               1, 1)
      } else {
        factor.mode <- factor(character.mode)
        unique.failure.modes <- unique(factor.mode[the.failed])
        factor.numbers <- as.character(as.numeric(unique.failure.modes))
        order.index <- order(factor.numbers)
        the.table <- data.frame(Number = factor.numbers[order.index],
                                Failure.Mode = unique.failure.modes[order.index])
        print(the.table)
        mode.plot.sym <- as.character(as.numeric(factor.mode))
      }
      xdelta <- 0.01 * (maxx - minx)
    } else {
      plot.failure.modes <- F
      mode.plot.sym <- rep("", length(the.censor.codes))
    }
    for (is in which.units.to.plot) {
      ypos <- ypos - ydelta
      the.ypos[is] <- ypos
      if (print.row)
        text(textx, ypos, paste(" ", iusys[is]), adj = 0, cex = 0.8)
      switch(the.censor.codes[is], `1` = , `5` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lwd = 2)
        if (plot.failure.modes) {
          points.default(y[is, 1] + xdelta, ypos, pch = mode.plot.sym[is],
                         cex = 0.7)
          if (circle.delta > 1/40) {
            if (is.R()) {
              symbols(y[is, 1] + xdelta, ypos, circles = 1,
                      add = T, inches = 3.5 * circle.delta *
                        0.7)
            } else {
              symbols(y[is, 1] + xdelta, ypos, circles = 1,
                      add = T, inches = 3.5 * circle.delta)
            }
          }
        } else {
          points.default(y[is, 1], ypos, pch = failpch,
                         cex = fail.cex, col = fail.col)
        }
      }, `2` = {
        arrows(0, ypos, y[is, 1], ypos, length = 0.1, lwd = 2)
      }, `3` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lty = 3, lwd = 2)
        text(y[is, 1], ypos, "|", adj = 0)
        text(y[is, 1]/2, ypos, paste("?", mode.plot.sym[is],
                                     sep = ""), adj = 0)
      }, `4` = {
        lines(c(0, y[is, 1]), c(ypos, ypos), lty = 1, lwd = 2)
        lines(c(y[is, 1], y[is, 2]), c(ypos, ypos), lty = 3,
              lwd = 2)
        text(y[is, 1], ypos, "|", adj = 0)
        text(y[is, 2], ypos, "|", adj = 0)
        if ((y[is, 2] - y[is, 1])/(SMRD2:::x.loc(1) - SMRD2:::x.loc(0)) >
            0.02) text((y[is, 1] + y[is, 2])/2, ypos, paste("?",
                                                            mode.plot.sym[is], sep = ""), adj = 0)
      }, stop("Unrecognized censor code", the.censor.codes[is],
              "in row", is))
      if (!count.on.right) {
        the.case.weight <- as.character(the.case.weights[is])
        if (the.case.weight == "1")
          the.case.weight <- ""
        text(y[is, ncol(y)] + horiz.inc, ypos + ypos.inc,
             the.case.weight, adj = 0, cex = 0.8)
      }
    }
    if (!all(the.case.weights == 1) && count.on.right) {
      the.case.weights <- as.character(the.case.weights)
      if (suppress.ones && length(the.case.weights[the.case.weights ==
                                                   1])/length(the.case.weights) > 0.1)
        the.case.weights[the.case.weights == 1] <- ""
      
      mtext(the.case.weights[which.units.to.plot], side = 4,
            at = the.ypos[which.units.to.plot], adj = 1,
            line = 1.5, las = 1, cex = 0.8)
      
      mtext(side = 4, at = max(ylim)*1.03, expression(underline(bold("Count"))), 
            line = 1.5, las = 1, cex = 0.8, adj = 1)
      
      #text(x.loc(1.03), y.loc(1.04), expression(underline(bold("Count"))))
    }
    if (print.row)
      #text(x.loc(0.01), y.loc(0.97), expression(underline(bold("Row"))), adj = 0)
      SMRD2:::CheckPrintDataName()
    invisible()

}
            
})

#' Print the lifedata class to standard output.
#'
#' @param x A \code{lifedata} object.
#' @param distribution A character vector of distributions names
#' @param events A logical value. If \code{TRUE} an event plot is returned,
#'               otherwise an estimate of the cdf is returned.
#'
#' @return NULL. Prints to standard out.
#'
#' @name print
#' @aliases print print,lifedata-method
#' @docType methods
#' @rdname print-methods
#' @export
#'
#' @seealso \code{\link{print}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' print(lz.ld)
#' lz.ld
#' getMethod("print", "lifedata")
#' }
setMethod("print", 
          signature = "lifedata",
          definition = function(x, 
                                includex = T, 
                                quote = F, 
                                prefix = "", 
                                digits = 4,...){

    cat(paste("Data from: ", get.data.title(x@frame), "\n"))
            
    obs.type <- c("Dummy", 
                  "Failure", 
                  "R-Censored", 
                  "L-Censored",
                  "Interval", 
                  "Small-Interval")
    
    trun.type <- c("None", "Right", "Left", "Interval")
    the.case.weights <- case.weights(x@frame)
    the.censor.codes <- censor.codes(x@frame, fill.in = F)
    
    if(is.null(the.case.weights) || all(the.case.weights == 1)) {
      
       name.case.weights <- NULL
       the.case.weights <- NULL
        
     } else {
    
       name.case.weights <- "Case.weights"
    
     }
    
    if(is.null(failure.modes(x@frame))) {
      
       name.fail.modes <- NULL
       the.failure.modes <- NULL
       
     } else {
       
       name.fail.modes <- "Failure.Modes"
       the.failure.modes <- as.character(failure.modes(x@frame))
       
     }
    
    if(is.null(the.censor.codes) || all(the.censor.codes == 1)) {
      
       name.censor.code <- NULL
       status <- NULL
       if(all(the.censor.codes == 1)) the.censor.codes <- NULL
       
     } else {
       
       name.censor.code <- "Status"
       status.ind <- match(censor.codes(x@frame), 0:5)
       status <- obs.type[status.ind]
       
     }
    
    if(is.null(truncation.codes(x@frame))) {
      
       name.truncation.code <- NULL
       name.truncation.resp <- NULL
       trunc.status <- NULL
        
     } else {
       
       name.truncation.code <- "TrunCode"
       name.truncation.resp <- colnames(truncation.response(x@frame))
       trunc.ind <- match(truncation.codes(x), 1:4)
       trunc.status <- trun.type[trunc.ind]
       
     }
    
    the.frame <- data.frame(cbind(Response(x@frame), 
                                  the.censor.codes,
                                  the.case.weights, 
                                  the.failure.modes, 
                                  trunc.status, 
                                  truncation.response(x@frame)))
    
    if(!is.null(xmat(x@frame))) {
      
       if(dimnames(xmat(x@frame))[[2]][1] == "(Intercept)") {
         
          xmat(x@frame) <- xmat(x@frame)[, -1, drop = F]
          
       }
      
       the.frame <- data.frame(the.frame, xmat(x@frame))
       
    }
    
    names(the.frame) <- c(colnames(Response(x@frame)), 
                          name.censor.code,
                          name.case.weights, 
                          name.fail.modes, 
                          name.truncation.code,
                          name.truncation.resp, 
                          dimnames(xmat(x@frame))[[2]])
    
    the.frame$Status <- status
    print.data.frame(the.frame)
    #invisible(the.frame)
    
})

#' Print the lifedata class to standard output.
#'
#' @param x A \code{lifedata} object.
#' @param distribution A character vector of distributions names
#' @param events A logical value. If \code{TRUE} an event plot is returned,
#'               otherwise an estimate of the cdf is returned.
#'
#' @return NULL. Prints to standard out.
#'
#' @name show
#' @aliases show show,lifedata-method
#' @docType methods
#' @rdname show-methods
#' @export
#'
#' @seealso \code{\link{show}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' show(lz.ld)
#' lz.ld
#' getMethod("show", "lifedata")
#' }
setMethod("show", 
          signature = "lifedata",
          definition = function(object){ print(object) })


#' Generate and print a summary from a lifedata object to standard output.
#'
#' @param x A \code{lifedata} object.
#' @param distribution A character vector of distributions names
#' @param events A logical value. If \code{TRUE} an event plot is returned,
#'               otherwise an estimate of the cdf is returned.
#'
#' @return NULL. Prints to standard out.
#'
#' @name show
#' @aliases summary summary,lifedata-method
#' @docType methods
#' @rdname summary-methods
#' @export
#'
#' @seealso \code{\link{summary}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' summary(lz.ld)
#' superalloy.ld <- life_data(superalloy,
#'                            response.column = 1, 
#'                            censor.column = 2,
#'                            x.columns = c(5,6,4),
#'                            time.units = "Kilocycles")
#' summary(superalloy.ld)                        
#' getMethod("summary", "lifedata")
#' }
setMethod("summary", 
          signature = "lifedata",
          definition = function(object, 
                                printem = T, 
                                print.limit = 50,...){ 

lda.type <- SMRD2:::data.object.type(data.ld = object@frame)

if(lda.type != "frame.centered") cat("\nOld-style life data object; consider rebuilding it\n")
data.ld <- SMRD:::SMRD.sanity.life.data(x = object@frame)
the.response <- SMRD2:::Response(data.ld)
number.cases <- nrow(the.response)
the.case.weights <- SMRD2:::case.weights(data.ld)

if(is.null(the.case.weights)) the.case.weights <- rep(1, number.cases)

the.censor.codes <- SMRD2:::censor.codes(data.ld)
if(is.null(the.censor.codes)) {
   
   the.censor.codes <- rep(1, number.cases)
   no.censoring <- T
   
 } else {
   
   no.censoring <- F
   
}

not.dummy <- the.case.weights > 0 & the.censor.codes > 0

if(printem) {
  
   out <- list()
   
   summary_title <- paste0("Summary of ", SMRD2:::get.data.title(data.ld))
   row_names <- c(NULL)
   vals <- c(NULL)
  
   data.note <- SMRD2:::get.data.note(data.ld)
   
   if(!is.null(data.note)) {
      
      the.characters <- SMRD2:::string2char(data.note)
      print.note <- length(the.characters) > 1 && !all(the.characters == "")
      if(print.note) {
        
        row_names <- c(row_names, SMRD2:::parse.note(data.note))
        vals <- c(vals,'')
        
      }
      
   }
   
   row_names = c(row_names,"Rows in data matrix: ") ; vals = c(vals,nrow(the.response))
   row_names = c(row_names,"Response units: "); vals = c(vals, SMRD2:::get.time.units(data.ld))
   row_names = c(row_names,"Minimum response: "); vals = c(vals, format(min(the.response)))
   row_names = c(row_names,"Maximum response: "); vals = c(vals, format(max(the.response)))
   row_names = c(row_names,"Cases in data set: ") ; vals = c(vals, sum(the.case.weights[not.dummy]))
   
   number.exact.fail <- sum(the.case.weights[not.dummy & the.censor.codes == 1])
   
   if(number.exact.fail > 0) {
     
      row_names = c(row_names, "Exact observations: ")
      vals = c(vals, number.exact.fail)
     
   }
   
   number.right.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 2])
   if(number.right.censored > 0) {
     
      row_names = c(row_names, "Right censored observations: ")
      vals = c(vals, number.right.censored)
     
   }
   
   number.left.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 3])
   if(number.left.censored > 0) {
     
      row_names = c(row_names, "Left censored observations: ")
      vals = c(vals, number.left.censored)
     
   }
   
   number.interval.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 4])
   if(number.interval.censored > 0) {
     
      row_names = c(row_names, "Interval censored observations: ")
      vals = c(vals, number.interval.censored)
     
   }
   
   number.sinterval.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 5])
   if(number.sinterval.censored > 0) {
     
      row_names = c(row_names, "Small-interval observations: ") ; vals = c(vals, number.sinterval.censored)
     
   }
   
   if(no.censoring) {
     
      row_names = c(row_names, "Censoring information: ")
      vals = c(vals,'none')
     
   }
   
   the.failure.modes <- SMRD2:::failure.modes(data.ld)
   if(!is.null(the.failure.modes)) {
     
      row_names = c(row_names, "Unique failure modes: ")
      vals = c(vals, paste(unique(as.character(the.failure.modes)),collapse = ", "))
     
   }
   
   truncation.codes <- SMRD2:::truncation.codes(data.ld)
   ty <- SMRD2:::truncation.response(data.ld)
   
   if(!is.null(truncation.codes) && !is.null(ty)) {
     
      if(is.null(truncation.codes) || is.null(ty)) {
        
         stop("If either truncation.codes or ty is specified, both must be specified")
        
      }
      if(length(truncation.codes) != number.cases) {
        
         stop(paste("Number of truncation codes ",length(truncation.codes), " is wrong"))
        
      }
      ty <- as.matrix(ty)
      if (nrow(ty) != number.cases) {
        
          stop(paste("Number of truncation times ",length(ty), " is wrong"))
        
      }
      nty <- ncol(ty)
      row_names = c(row_names, "Right truncated observations: ")
      vals = c(vals,  sum(the.case.weights[not.dummy & truncation.codes == 2]))
      row_names = c(row_names, "Left truncated observations: ") 
      vals = c(vals, sum(the.case.weights[not.dummy & truncation.codes == 3]))
      row_names = c(row_names, "Interval truncated observations: ")
      vals = c(vals, sum(the.case.weights[not.dummy & truncation.codes == 4]))
      
    } else {
      
      row_names = c(row_names, "Truncation information: ")
      vals = c(vals,'none')
      nty <- 0
      truncation.codes <- rep(1, length(the.censor.codes))
      ty <- rep(0, length(the.censor.codes))
      
    }
   
    the.xmat <- SMRD2:::xmat(data.ld)
    if(is.null(the.xmat)) {
       
       row_names = c(row_names,"Explanatory variables: ")
       vals = c(vals,'none')
      
     } 
    
     out[[summary_title]] <- data.frame(row_names,
                                        vals, 
                                        row.names = NULL,
                                        stringsAsFactors = F) 
     
     colnames(out[[summary_title]]) <- NULL
    
    if(!is.null(the.xmat)) {
       
       if(nrow(the.xmat) < 200) {
         
          x.strings <- apply(the.xmat, 1, paste, collapse = " ")
          uniquex <- unique(x.strings)
          if (is.list(the.xmat)) {
            
              numeric.columns.list <- lapply(the.xmat, is.numeric)
              numeric.columns <- unlist(numeric.columns.list)
              
            } else {
              
              numeric.columns <- apply(the.xmat, 2, is.numeric)
              
            }
          
            if(any(numeric.columns)) {
              
               the.mean <- apply(the.xmat[, numeric.columns,drop = F], 2, mean)
               the.sd <- sqrt(apply(the.xmat[, numeric.columns, drop = F], 2, var))
               the.cv <- the.sd/the.mean
               predictors <- names(SMRD2:::get.xlabel(data.ld)[numeric.columns])
               xsummary <- data.frame(predictors,
                                      apply(the.xmat[, numeric.columns,drop = F], 2, min), 
                                      apply(the.xmat[, numeric.columns,drop = F], 2, max), 
                                      the.mean, 
                                      the.sd, 
                                      the.cv,
                                      stringsAsFactors = F)

               colnames(xsummary) <- c("predictor","min", "max", "mean", "sd", "cv")
               out[["Summary of numeric columns in X matrix"]] <- xsummary
               
            }
          
            ncolx <- ncol(the.xmat)
            c1 <- rep(1, length(uniquex))
            the.table <- data.frame(the.xmat[1:length(uniquex),], c1, c1, c1, c1, c1, c1, c1, c1)
            
            for(i in 1:length(uniquex)) {
              
                the.stuff <- uniquex[i] == x.strings
                the.table[i, ncolx + 1] <- min(the.response[the.stuff])
                the.table[i, ncolx + 2] <- max(the.response[the.stuff])
                the.table[i, ncolx + 3] <- mean(the.response[the.stuff])
                the.sd <- sqrt(var(the.response[the.stuff]))
                if (is.na(the.sd)) the.sd <- 0
                the.table[i, ncolx + 4] <- the.sd
                the.table[i, ncolx + 5] <- sum(the.case.weights[the.stuff & the.censor.codes == 1])
                the.table[i, ncolx + 6] <- sum(the.case.weights[the.stuff & the.censor.codes == 2])
                the.table[i, ncolx + 7] <- sum(the.case.weights[the.stuff & the.censor.codes == 3])
                the.table[i, ncolx + 8] <- sum(the.case.weights[the.stuff & the.censor.codes == 4])
                the.table[i, ncolx + 9] <- sum(the.case.weights[the.stuff])
                the.ones <- (1:nrow(the.response))[the.stuff]
                
                for(j in 1:ncolx) {
                  
                    if(is.factor(the.xmat[the.ones[1], j])) {
                      
                       the.table[i, j] <- as.character(the.xmat[the.ones[1],j])
                       
                     } else { 
                       
                       the.table[i, j] <- the.xmat[the.ones[1],j]  
                      
                     }
                  
                }
                    
           }
                unique_combinations <- 1:nrow(the.table)
                the.table <- cbind(unique_combinations, the.table)
                colnames(the.table) <- c("",
                                         names(SMRD2:::get.xlabel(data.ld)),
                                         "Min response", 
                                         "Max response", 
                                         "Mean response",
                                         "SD response",
                                         "Exact", 
                                         "R-cen", 
                                         "L-cen", 
                                         "Int-cen",
                                         "Total")
                
                check.zero <- function(x) { any(x != 0) }
                some.censoring <- any(the.table[, ncolx + 9] != the.table[, ncolx + 5])
                if(some.censoring) the.table <- the.table[, -(ncolx + c(3, 4))]
                any.non.zero <- apply(the.table, 2, check.zero)
                
                if(nrow(the.table) < print.limit) { 
                  
                   out[["Unique X conditions"]] <- the.table[, any.non.zero]
                   
                 } else {
                   
                   out[["Unique X conditions"]] <- paste0("Unique X conditions: ", nrow(the.table))
                   
                 }
            }
      }
}

    print(out, row.names = F)

    results <- list(number.cases = sum(the.case.weights[not.dummy]))
    invisible(results)
            
})
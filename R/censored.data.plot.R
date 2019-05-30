#' Plot failures and censored observations as a function of the response
#'
#' @param data.ld Name (as a character string) of a \code{life.data} object created using the function \code{frame.to.ld}
#' @param explan.var numeric(\code{1})
#' 
#'    Explanatory variable to be plotted versus the responses.  Value cannot be greater that the number \code{x.columns} defined in \code{data.ld}.
#'    Setting \code{explan.var = 1} selects the \code{x.column} with the lowest number, regardless of the order in which the \code{x.columns} were defined in \code{frame.to.ld}.  For example, \code{data.ld} can have argument \code{x.columns = c(4,5,6)} or \code{x.columns = c(6,5,4)}.  For either case, \code{explan.var = 1} selects \code{x.column = 4} since this is the smallest value.
#'    Note, \code{explan.var} may also be defined using a character string matching the name of column
#' @param ylim numeric(\code{c(NA, NA)})
#' 
#'    \bold{(optional)} Range of plotting values for the y-axis.  If left blank, values will be chosen based on the information in \code{data.ld}
#' @param xlim numeric(\code{c(NA, NA)})
#' 
#'    \bold{(optional)} Range of plotting values for the x-axis.  If left blank, values will be chosen based on the information in \code{data.ld}
#' @param x.axis character(\code{"linear"}) 
#' 
#'    Transformation applied to the values defined along the x-axis.  See \code{.generic.relationship.name} for the list of transformation relationships that can be used
#' @param y.axis character(\code{"linear"}) 
#' 
#'    Transformation applied to the values defined along the x-axis.  See \code{.generic.relationship.name} for the list of transformation relationships that can be used.
#' @param my.title character(\code{NULL})
#' 
#'    Title to be displayed above the plot
#' @param ylab character(\code{get.time.units(data.ld)})
#' 
#'    \bold{(optional)} Title of the y-axis. If left blank, the value of the \code{time.units} argument defined in \code{data.ld} will be used.  If no \code{time.units} were defined when \code{data.ld} was created, \code{'Hours'} is used as the default

#' @param xlab  character(\code{NULL})
#' 
#'    \bold{(optional)} Title of the x-axis.  If left blank, the column name in \code{data.ld} corresponding the argument \code{explan.var} is used. 
#' @param cex numeric(\code{par()$cex})
#' @param cex.labs numeric(\code{par()$cex.lab})
#' @param cex.points numeric(\code{1.2})
#' 
#'    Scaling value for size of the points to be plotted
#' @param add logical(\code{FALSE}) 
#' 
#'    If \code{TRUE} plotting is suppressed to allow for the addition of more graphical features
#' @param grids logical(\code{FALSE})
#' 
#'    If \code{TRUE}, gridlines are provided on the plot
#' @param title.option character(\code{GetSMRDDefault("SMRD.TitleOption")})
#' 
#'    \bold{(optional)} Produces a title above the plot using the SMRD default title format 

#' @param response.on.yaxis character(\code{TRUE})
#' 
#'    If \code{FALSE}, the x and y axes are reversed such that the response values are plotted along the x-axis  

#'
#' @author William Q. Meeker, PhD
#' @export
#'
#' @seealso \code{\link{frame.to.ld}}
#' @examples 
#' # Create a \code{life.data} object
#' superalloy.ld <- frame.to.ld(superalloy,
#'                              response.column = 1, 
#'                              censor.column = 2,
#'                              x.columns = c(5,6,4),
#'                              time.units = "Kilocycles")
#'
#'summary(superalloy.ld)
#'
#'censored.data.plot(superalloy.ld, 
#'                   explan.var = 3)
#'
#'censored.data.plot(superalloy.ld, 
#'                   explan.var = 3 ,
#'                   response.on.yaxis = F)
#'
#'censored.data.plot(superalloy.ld, 
#'                   explan.var = 3, 
#'                   x.axis = "log", 
#'                   y.axis = "log")
#'
#'censored.data.plot(superalloy.ld, 
#'                   explan.var = 3 ,
#'                   response.on.yaxis = F, 
#'                   x.axis = "log", 
#'                   y.axis = "log")
censored.data.plot <-
function (data.ld, 
          explan.var = 1, 
          ylim = c(NA, NA), 
          xlim = c(NA,NA), 
          x.axis = "linear", 
          y.axis = "linear", 
          my.title = NULL,
          ylab = get.time.units(data.ld), 
          xlab = NULL, 
          cex = par()$cex, 
          cex.labs = par()$cex.lab,
          cex.points = 1.2, 
          add = F, 
          grids = F, 
          title.option = GetSMRDDefault("SMRD.TitleOption"),
          response.on.yaxis = T)
{
    y <- Response(data.ld)
    number.cases <- nrow(y)
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    the.initial.xmat <- xmat(data.ld)
    x.axis.name <- x.axis
    
    if (length(explan.var) > 1) {
        
        warning("Length of explan.var =", length(explan.var), "Using the first one only")
        explan.var <- explan.var[1]
        
    }
    xmat.names <- dimnames(the.initial.xmat[, explan.var, drop = F])[[2]]
    
    `if`(is.character(explan.var),
         var.number <- match(explan.var, xmat.names),
         var.number <- explan.var)
    
    x.name <- get.xlabel(data.ld)[var.number]
    if (is.null(xlab)) {
        if (generic.relationship.name(x.axis.name) == "Box-Cox") {
            the.power <- attr(x.axis.name, "the.power")
            x.axis.name <- paste(x.axis.name, "(", the.power,")", sep = "")
        }
        
        `if`(x.axis.name == "linear",
             xlab <- paste(x.name),
             xlab <- paste(x.name, "on", x.axis.name, "Scale"))
        
    }
    if (generic.relationship.name(x.axis) == "Arrhenius") {
        x.axis <- "Arrhenius3"
        x.axis <- set.relationship.power(x.axis, power)
    }
    y.axis.in <- y.axis
    if (generic.relationship.name(y.axis) == "Arrhenius") {
        y.axis <- "Arrhenius3"
        y.axis <- set.relationship.power(y.axis, power)
    }
    if (is.null(my.title)) my.title <- get.data.title(data.ld)
    if (!is.null(title.option) && length(title.option) > 0 && title.option == "blank") my.title <- ""
    if (length(explan.var) > 1) stop("Only one explan.var at a time in call")
    
    check.column(explan.var, 
                 ncol(the.initial.xmat), 
                 dimnames(the.initial.xmat)[[2]],
                 number.col.allowed = 1)
    
    axis.labels <- NULL
    the.xmat <- the.initial.xmat[, explan.var, drop = F]
    n <- ncol(the.xmat)
    
    for(i in seq(n)) {
        
        if(is.matrix(the.xmat[, i, drop = F])) {
            
           x <- xpdmat.data.frame(the.xmat)
           n <- ncol(the.xmat)
            
        }
        
    }
    minlength <- c(4, 7)
    dolabel <- `if`(is.vector(minlength) && length(minlength) == 2,
                    function(x, minlength) abbreviate(x, minlength = minlength),
                    stop("bad argument for abbreviate"))
    
    xrange <- axis.labels <- list()
    
    for (i in seq(n)) {
        
         X <- the.xmat[, i]
        
         if(is.factor(X)) {
            
            the.xmat[, i] <- I(factor(X, exclude = `if`(any(is.na(X)),NA, NULL)))
            axis.labels[[i]] <- dolabel(levels(the.xmat[, i]), minlength)
            xrange[[i]] <- c(0, max(the.xmat[, i], na.rm = T) + 1)
           
          } else {  
    
            xrange[[i]] <- range(X, na.rm = T)
            axis.labels <- unlist(axis.labels) 
           
          }
    }
    
    yrna <- is.na(ylim)
    if (any(yrna)) ylim[yrna] <- range(y)[yrna]
    xrna <- is.na(xlim)
    if (any(xrna)) xlim[xrna] <- range(as.vector(the.xmat[, 1]))[xrna]
    
    relationship.sanity(the.xmat[, 1, drop = F], x.axis, "Transformation for explan var")
    relationship.sanity(y[, 1, drop = F], y.axis, "Transformation for the response")
    if (ncol(y) == 2) relationship.sanity(y[, 2, drop = F], y.axis, "Transformation for the response")
    the.xmat <- f.relationship(the.xmat[, 1, drop = F], x.axis)
    ymat <- f.relationship(y, y.axis)
    
    if(!add) plot.paper(x = xlim, 
                        y = ylim, 
                        x.axis = x.axis,
                        y.axis = y.axis, 
                        ylab = ylab, 
                        xlab = xlab, 
                        response.on.yaxis = response.on.yaxis,
                        cex = cex, 
                        cex.labs = cex.labs, 
                        grids = grids, 
                        xaxis.labels = axis.labels)

    mtext(text = my.title, side = 3, cex = cex, line = 1)
    dummy <- the.censor.codes == 0 | the.case.weights == 0
    plot.censored.data.points(x = the.xmat[, 1], 
                              y.data = ymat,
                              censor.codes = the.censor.codes, 
                              x.axis = x.axis, 
                              y.axis = y.axis,
                              cex.points = cex.points, 
                              cex = cex, 
                              response.on.yaxis = response.on.yaxis,
                              pch.point = 16, 
                              dummy = dummy)
    
    invisible()
}

#' An S4 class to represent a CDFEST object.
#'
#' @slot frame A \code{data.frame} class object
CDFEST <- setClass("CDFEST",
                     slots = c(rlist = "list"))


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
#' @aliases print print,CDFEST-method
#' @docType methods
#' @rdname print-methods
#' @export
#'
#' @seealso \code{\link{print}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' lz.cd <- cdfest(lz.ld, distribution = 'weibull')
#' print(lz.cd)
#' lz.cd
#' getMethod("print", "CDFEST")
#' }
setMethod("print", 
          signature = "CDFEST",
          definition = function(x, 
                                conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
                                band.type = "pointwise", 
                                a.limit = 0.001, 
                                b.limit = 0.999,
                                digits = GetSMRDDefault("SMRD.DigitsPrinted"), 
                                quote = T,
                                prefix = "",...){

    old <- options(digits = digits)
    on.exit(options(old))
    if (band.type == "none") band.type <- "pointwise"
    time.units <- get.time.units(x@rlist$data.ld)
    line1<-paste("Nonparametric estimates from", get.data.title(x@rlist$data.ld), sep = " ")
    x@rlist$p[x@rlist$p < 0] <- 0
    the.bands <- list()
    conf.char <- percent.conf.level(conf.level)
    extra.names <- NULL
    if (!is.null(x@rlist$sd)) {
        
        extra.names <- c("SE_Fhat", 
                         paste(conf.char, "Lower"),
                         paste(conf.char, "Upper"))
        
        the.bands <- get.npbands(x@rlist, 
                                 band.type, 
                                 conf.level = conf.level,
                                 how.show.interval = "step.fun", 
                                 a.limit = a.limit,
                                 b.limit = b.limit)
        
        line2 <- paste(" with approximate ", paste(100 * conf.level,
            "%", sep = "")," ", band.type, " confidence intervals.", sep = "")
    }
    the.text  <- paste(line1, line2, sep = "")
    the.table <- cbind(x@rlist$p, 
                       x@rlist$q, 
                       x@rlist$prob, 
                       x@rlist$sd, 
                       the.bands$lower,
                       the.bands$upper)
    
    colnames(the.table) <- c(paste(time.units, "-lower", sep = ""), 
                             paste(time.units, "-upper", sep = ""), "Fhat", extra.names)
    prlist <- list()
    prlist$text  <- the.text
    prlist$table <- the.table

    print(prlist)

})

#' Print the CDFEST class to standard output.
#'
#' @param x A \code{CDFEST} object.
#' @param distribution A character vector of distributions names
#' @param events A logical value. If \code{TRUE} an event plot is returned,
#'               otherwise an estimate of the cdf is returned.
#'
#' @return NULL. Prints to standard out.
#'
#' @name show
#' @aliases show show,CDFEST-method
#' @docType methods
#' @rdname show-methods
#' @export
#'
#' @seealso \code{\link{show}}
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' lz.cd <- cdfest(lz.ld, distribution = 'weibull')
#' show(lz.cd)
#' lz.cd
#' getMethod("show", "CDFEST")
#' }
setMethod("show", 
          signature = "CDFEST",
          definition = function(object){ print(object) })

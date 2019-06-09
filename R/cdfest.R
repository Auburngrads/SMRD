#' Generate a CDF estimate
#'
#' @param data.ld 
#' @param gamthr 
#' @param kprint 
#' @param maxit 
#' @param tol 
#' @param maxmsd 
#' @param start.values 
#' @param debug1 
#'
#' @return A formal S4 object of class CDFEST with slot \code{list}
#' @export
#'
#' @examples
#' \dontrun{
#' lz.ld <- life_data(lzbearing, response.column = 1)
#' lz.cd <- cdfest(lz.ld, distribution = 'weibull')
#' lz.cd
#' }

cdfest <-
function (data.ld, 
          gamthr = 0, 
          kprint = 0, 
          maxit = 5e+05, 
          tol = 0.001,
          maxmsd = 200, 
          start.values = NULL,
          debug1 = F,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          band.type = "pointwise", 
          a.limit = 0.001, 
          b.limit = 0.999,
          digits = GetSMRDDefault("SMRD.DigitsPrinted"), 
          quote = T,...)
{
    y <- Response(data.ld)
    the.case.weights <- case.weights(data.ld)
    
    if (is.null(start.values)) nstart <- 0
    number.cases <- nrow(y)
    ny <- ncol(y)
    the.censor.codes <- censor.codes(data.ld)
    
    if (length(gamthr) == 1) gamthr <- rep(gamthr, number.cases)
    
    if (length(gamthr) != number.cases) stop("specified offset is the wrong length")
    
    if (ny == 2) gamthr <- cbind(gamthr, gamthr)
    
    y <- y - as.matrix(gamthr)
    
    number.observations <- sum(the.case.weights[the.censor.codes > 0])
    left.trun.cond <- NULL
    right.trun.cond <- NULL
    the.truncation.codes <- truncation.codes(data.ld)
    
    if (is.null(the.truncation.codes)) {
      
        ty <- 1
        nty <- 0
        the.truncation.codes <- 1
        
        } else {
          
        ty <- truncation.response(data.ld)
        nty <- ncol(ty)
        if (all(the.truncation.codes == 3)) left.trun.cond <- min(ty[the.truncation.codes == 3, 1])
        if (all(the.truncation.codes == 2)) right.trun.cond <- max(ty[the.truncation.codes == 2, nty])
    }
    dummy <- the.censor.codes
    ndscrat <- 3 * number.cases + 4
    nrscrat <- max(7 * (number.cases + 1), 
                   (maxmsd * (maxmsd - 1))/2 + 1)
    niscrat <- 6 * number.cases + 7
    
    if (debug1) browser()
    
    zout <- WQMCDFEST(as.matrix(y),
                      as.integer(ny),
                      as.integer(the.censor.codes),
                      as.integer(the.case.weights),
                      as.matrix(ty),
                      as.integer(nty),
                      as.integer(the.truncation.codes),
                      as.integer(number.cases),
                      as.integer(nstart),
                      double(ndscrat),
                      double(nrscrat),
                      integer(niscrat),
                      as.integer(kprint),
                      as.integer(maxit),
                      as.double(tol),
                      as.integer(maxmsd),
                      p = double(number.cases + 1),
                      q = double(number.cases + 1),
                      prob = double(number.cases + 1),
                      sd = double(number.cases + 1),
                      m = integer(1),
                      pchmax = double(1),
                      lsd = integer(1),
                      ier = integer(1))
    
    lsd <- as.logical(zout$others$lsd)
    m <- zout$others$m
    
    if (zout$others$ier > 0) {
      
        if (zout$others$ier != 21 || map.SMRDDebugLevel() >= 4)
            warning(paste("Cdfest error message", zout$others$ier,
                collapse = " "))
    }
    
    q <- zout$numvec$q
    p <- zout$numvec$p
    prob <- zout$numvec$prob
    length(q) <- m
    length(p) <- m
    length(prob) <- m
    
    if (lsd) {
      
        sd <- zout$numvec$sd
        length(sd) <- m
        rlist <- list(data.ld = data.ld, 
                      p = p, 
                      q = q, 
                      prob = prob,
                      sd = sd, 
                      number.observations = number.observations,
                      left.trun.cond = left.trun.cond, 
                      right.trun.cond = right.trun.cond)
  
     } else {
          
        rlist <- list(data.ld = data.ld,
                      p = p, 
                      q = q, 
                      prob = prob,
                      number.observations = number.observations, 
                      left.trun.cond = left.trun.cond,
                      right.trun.cond = right.trun.cond)
        
        }
    
    old <- options(digits = digits)
    on.exit(options(old))
    if (band.type == "none") band.type <- "pointwise"
    time.units <- get.time.units(rlist$data.ld)
    line1 <- paste("Nonparametric estimates from", get.data.title(rlist$data.ld), sep = " ")
    rlistp <- rlist$p
    rlistp[rlistp < 0] <- 0
    the.bands <- list()
    conf.char <- percent.conf.level(conf.level)
    extra.names <- NULL
    if (!is.null(rlist$sd)) {

        extra.names <- c("SE_Fhat",
                         paste(conf.char, "Lower"),
                         paste(conf.char, "Upper"))

        the.bands <- get.npbands(rlist,
                                 band.type,
                                 conf.level = conf.level,
                                 how.show.interval = "step.fun",
                                 a.limit = a.limit,
                                 b.limit = b.limit)

        line2 <- paste(" with approximate ", paste(100 * conf.level,
            "%", sep = "")," ", band.type, " confidence intervals.", sep = "")
    }
    the.text  <- paste(line1, line2, sep = "")
    the.table <- cbind(rlistp,
                       rlist$q,
                       rlist$prob,
                       rlist$sd,
                       the.bands$lower,
                       the.bands$upper)

    colnames(the.table) <- c(paste(time.units, "-lower", sep = ""),
                             paste(time.units, "-upper", sep = ""), "Fhat", extra.names)
    rlist$text  <- the.text
    rlist$table <- the.table
    
    oldClass(rlist) <- "cdfest"
    return(rlist)


}

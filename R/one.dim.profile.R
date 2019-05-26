#' Title
#'
#' @param gmle.out 
#' @param profile.setup 
#' @param profile.stable.parameters 
#' @param profile.on.list 
#' @param special.stuff.profile 
#' @param range.list 
#' @param size 
#' @param interactive 
#' @param save.structures 
#' @param addname 
#' @param save.parameter.vectors 
#' @param debug1 
#' @param plot.em 
#' @param print.ci 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' R4490.rdu <- frame.to.rdu(r4490,
#'                           ID.column = "vin",
#'                           time.column = "days" , 
#'                           cost.count.column = "costcount" ,
#'                           event.column = "code")
#' 
#' attr(R4490.rdu,"WindowInfo")
#' 
#' event.plot(R4490.rdu)
#' R4490.mcf <- mcf(R4490.rdu)
#' plot(R4490.mcf)
#' 
#' R4490.nhpp.out <- PlotMCFandNHPP(R4490.rdu, form = "power rule")
#' one.dim.profile(R4490.nhpp.out,size = 5,save.s = T)
#' two.dim.profile(R4490.nhpp.out, 
#'                 profile.on.list = NULL,
#'                 which = c(1,2), 
#'                 size = c(5,5))
#' 
#' profile.contour(R4490.nhpp.outstruct1x2,
#'                 transformationy = "log",
#'                 variable.namey = "sigma", 
#'                 variable.namex = "mu", 
#'                 v = c(0.001, 0.01, .1,0.2, 0.4, 0.7, 0.9) )
#' 
#' }
one.dim.profile <-
function (gmle.out, 
          profile.setup = NULL, 
          profile.stable.parameters = NULL,
          profile.on.list = 1:length(theta.hat),
          special.stuff.profile = NULL,
          range.list = NULL, 
          size = 40, 
          interactive = F, 
          save.structures = T,
          addname = NULL, 
          save.parameter.vectors = F,
          debug1 = 0, 
          plot.em = T,
          print.ci = TRUE)
{
    old.options <- options()
    options(keep = NULL, digits = 5)
    on.exit(options(old.options))
    assign.gmle(gmle.out, debug1)
    assign(envir = .frame0,  inherits = TRUE,"special.stuff.profile", value = special.stuff.profile)
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    theta.hat <- gmle.out$est.out$x
    
    if (is.null(profile.stable.parameters)) {
      
        if (any(profile.on.list > length(theta.hat))) stop("Need profile.stable.parameters function")
      
        profile.stable.parameters <- function(x.theta.hat, profile.on) {
            theta.hat <- x.theta.hat
            return(theta.hat)
        }
    }
    if (is.null(profile.setup)) {
      
        if (any(profile.on.list > length(theta.hat))) stop("Need profile.setup function")
      
        profile.setup <- function(theta.hat, 
                                  t.profile.names,
                                  profile.on) {
          
            return(list(profile.name = t.profile.names[profile.on],
                        h.theta.hat = theta.hat, 
                        profile.on.pos = profile.on,
                        ktran = rep(1, length(profile.on))))
        }
    }
    assign(envir = .frame0,  inherits = TRUE,"profile.stable.parameters", value = profile.stable.parameters)
    t.param.names <- gmle.out$model$t.param.names
    
    for (profile.index in 1:length(profile.on.list)) {
      
         profile.on <- profile.on.list[profile.index]
        
         `if`(is.null(range.list[[profile.index]]),
              xlim <- profile.range(gmle.out, profile.on, profile.setup),
              xlim <- range.list[[profile.index]])

          profile.setup.out <- profile.setup(theta.hat, 
                                            t.param.names,
                                            profile.on)

        profile.name <- profile.setup.out$profile.name
        
        if (!interactive || ask.if(paste("Profile likelihood plot on", profile.name, "? "))) {
            structx1 <- y.eval(gmle.out, 
                               profile.setup.out,
                               xlim,
                               profile.on = profile.on, 
                               size = size, 
                               save.parameter.vectors = save.parameter.vectors)
            
            if (monitor >= 3) print(structx1)
            
            oldClass(structx1) <- "one.dim.out"
            
            if (save.structures) {
                data.set.name <- deparse(substitute(gmle.out))
                structure.name <- paste(data.set.name, "struct",
                  profile.on, addname, sep = "", collapse = "")
                #cat("Saving", structure.name, "\n")
                assign(envir = .frame0,  inherits = TRUE,structure.name, structx1)
            }
            if (names(dev.cur()) != "null device" && plot.em) profile.plot(structx1, print.ci = print.ci)
        }
    }
    
    invisible(structx1)
    
}

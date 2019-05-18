#' Title
#'
#' @param gmle.out 
#' @param profile.setup 
#' @param profile.stable.parameters 
#' @param range.list 
#' @param profile.on.list 
#' @param which 
#' @param special.stuff.profile 
#' @param size 
#' @param interactive 
#' @param save.structures 
#' @param addname 
#' @param monitor 
#' @param debug1 
#' @param alt.max 
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
two.dim.profile <-
function (gmle.out, profile.setup = NULL, profile.stable.parameters = NULL,
    range.list = NULL, profile.on.list = NULL, which = seq(1:length(t.param.names)),
    special.stuff.profile = NULL, size = c(10, 10), interactive = F,
    save.structures = T, addname = NULL, monitor = 1,debug1= 0,
    alt.max = F)
{
    old.options <- options()
    options(keep = NULL, digits = GetSMRDDefault("SMRD.DigitsPrinted"))
    on.exit(options(old.options))
    assign(envir = .frame0,  inherits = TRUE,"special.stuff.profile", value = special.stuff.profile)
    assign.gmle(gmle.out, debug1, monitor)
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    theta.hat <- gmle.out$est.out$x
    if (is.null(profile.stable.parameters)) {
        if (!is.null(profile.on.list) && any(unlist(profile.on.list) >
            length(theta.hat)))
            stop("Need profile.stable.parameters function")
        profile.stable.parameters <- function(x.theta.hat, profile.on) {
            theta.hat <- x.theta.hat
            return(theta.hat)
        }
    }
    if (is.null(profile.setup)) {
        if (!is.null(profile.on.list) && any(profile.on.list >
            length(theta.hat)))
            stop("Need profile.setup function")
        profile.setup <- function(theta.hat, t.profile.names,
            profile.on) {
            return(list(profile.name = t.profile.names[profile.on],
                h.theta.hat = theta.hat, profile.on.pos = profile.on,
                ktran = rep(1, length(profile.on))))
        }
    }
    assign(envir = .frame0,  inherits = TRUE,"profile.stable.parameters", value = profile.stable.parameters)
    t.param.names <- gmle.out$model$t.param.names
    if (is.null(profile.on.list)) {
        profile.on.list <- my.subsets(length(which), 2)
        for (i in 1:length(profile.on.list)) {
            this.one <- profile.on.list[[i]]
            profile.on.list[[i]] <- c(which[this.one[1]], which[this.one[2]])
        }
    }
    for (profile.index in 1:length(profile.on.list)) {
        profile.on <- profile.on.list[[profile.index]]
        xlim <- range.list[[profile.on[1]]]
        ylim <- range.list[[profile.on[2]]]
        if (is.null(xlim)) {
            xlim <- profile.range(gmle.out, profile.on[1],
                profile.setup)
        }
        if (is.null(ylim)) {
            ylim <- profile.range(gmle.out, profile.on[2],
                profile.setup)
        }
        profile.setup.out <- profile.setup(theta.hat, t.param.names,
            profile.on)
        if (monitor >= 3) {
            print(profile.setup.out)
        }
        profile.name <- profile.setup.out$profile.name
        if (!interactive || ask.if(paste("Profile likelihood plot on",
            profile.name, "? "))) {
            struct <- z.eval(gmle.out, profile.setup.out, xlim,
                ylim, profile.on = profile.on, size = size,
                alt.max = alt.max)
            struct$z[struct$z < 1e-05] <- 0
            oldClass(struct) <- "two.dim.out"
            if (save.structures) {
                data.set.name <- deparse(substitute(gmle.out))
                structure.name <- paste(data.set.name, "struct",
                  paste(profile.on, collapse = "x"), addname,
                  sep = "")
                cat("Saving output structure in:", structure.name,
                  "\n")
                assign(envir = .frame0,  inherits = TRUE,structure.name, struct)
            }
        }
    }
}

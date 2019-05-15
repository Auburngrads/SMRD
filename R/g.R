#' Title
#'
#' @param x 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ZelenCap.ld <- frame.to.ld(zelencap,
#'                            response.column = 1,
#'                            censor.column = 2,
#'                            case.weight.column = 3,
#'                            x.columns = c(4, 5),
#'                            time.units = "Hours")
#' 
#' ZelenCap.groupm.out2 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "Lognormal",
#'                      formula= Location ~ + g(celsius),
#'                      relationship = c("arrhenius", "log"))
#' 
#' 
#' ## Fitting to `volts` only
#' 
#' ZelenCap.groupm.out3 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "Lognormal",
#'                      formula= Location ~ + g(volts),
#'                      relationship = c("arrhenius", "log"))
#' 
#' ## Fitting to `celsius` and `volts`
#' 
#' ZelenCap.groupm.out4 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "normal",
#'                      formula = Location ~ + g(celsius) +  g(volts))
#' 
#' ZelenCap.groupm.out5 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "normal",
#'                      formula= Location ~  +  g(volts) + g(celsius))
#' 
#' ## Fitting to `volts` and `celsius` with interaction
#' 
#' ZelenCap.groupm.out6 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "normal",
#'                      formula = Location ~ celsius + volts + celsius:volts )
#' 
#' ## Omit the formula, use default of linear on all X's
#' 
#' ZelenCap.groupm.out7 <-
#'   groupm.mleprobplot(ZelenCap.ld,
#'                      distribution = "Lognormal",
#'                      relationship=c("arrhenius","log"))
#' 
#' ZelenCap.groupm.out8 <- 
#'   groupm.mleprobplot(ZelenCap.ld, 
#'                      distribution = "Lognormal", 
#'                      relationship = c("linear", "linear"), 
#'                      formula = Location ~ + g(volts) + g(celsius) + g(volts):g(celsius))
#' 
#' 
#' }
g <-
function (x)
{
    x.name <- deparse(substitute(x))
    if (is.factor(x) || is.character(x)) {
        return(as.factor(x))
    }
    the.relationship <- subscript.relationship(get(envir = .frame0,  "relationship.vector"), x.name)
    if (the.relationship == "class") {
        return(as.factor(x))
    }
    if (length(the.relationship) == 0) {
        warning(paste("ignored unrecognized relationship for",
            x.name))
        return(x)
    }
    f.relationship(x, the.relationship)
}

#' Title
#'
#' @param data.ld 
#' @param stress.var.list 
#' @param group.var 
#' @param the.ld.name 
#' @param prefix 
#' @param allow.poor.data 
#' @param number.needed 
#' @param printem 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' Bleed.ld <- frame.to.ld(bleed,
#'                         response.column = 1, 
#'                         censor.column = 2, 
#'                         case.weight.column = 3,
#'                         x.columns = 4,
#'                         time.units = "Hours")
#' 
#' Bleed.ld_D <- ld.split(Bleed.ld, stress.var = "D")
#' Bleed.ld_Other <- ld.split(Bleed.ld, stress.var = "Other")
#' 
#' event.plot(Bleed.ld_D)
#' summary(Bleed.ld_D)
#' 
#' event.plot(Bleed.ld_Other)
#' summary(Bleed.ld_Other)
#' 
#' plot(Bleed.ld,my.title="All Bases")
#' 
#' plot(Bleed.ld,
#'      distribution = "Weibull",
#'      my.title = "Bleed System Failures\nAll Bases")
#' 
#' plot(Bleed.ld_D,
#'      distribution = "Weibull",
#'      my.title = "Bleed System Failures\nOnly Base D")
#' 
#' plot(Bleed.ld_Other,
#'      distribution = "Weibull",
#'      my.title = "Bleed System Failures\nOmitting Base D")
#' }
ld.split <-
function (data.ld, stress.var.list = unique(xmat(data.ld)[, group.var]),
    group.var = 1, the.ld.name = NULL, prefix = NULL, allow.poor.data = F,
    number.needed = 2, printem = FALSE)
{
    all.stresses <- get.x.markers(data.ld, group.var = group.var)
    all.names <- get.x.markers(data.ld, group.var = group.var,
        long = T, collapse.on = ".")
    stress.names <- all.names[match(stress.var.list, all.stresses)]
    stress.names.comb <- paste(stress.names, collapse = ".")
    if (length(stress.names) == 1 && stress.names == "")
        stop(paste("The name", stress.var.list, "is not in the list of the levels of the specified variable",
            paste(group.var, collapse = " ")))
    if (is.null(the.ld.name)) {
        if (is.null(prefix))
            prefix <- GetDataPrefix(deparse(substitute(data.ld)))
        the.ld.name <- paste(prefix, ".", strip.blanks.nulls(stress.names.comb),
            ".ld", sep = "")
    }
    data.subset <- subset.life.data(x = data.ld, markers = stress.var.list,
        columns = group.var)
    poor.data <- !good.data(data.subset, number.needed = number.needed)
    if (!poor.data || allow.poor.data) {
      
    if (printem) cat("\nSaving subset data object", the.ld.name, "\n")
      
        assign(envir = .frame0,  inherits = TRUE,the.ld.name, data.subset)
    }
    if (poor.data) {
        warning(paste("Fewer than", number.needed, "failures in subset",
            stress.names.comb))
      
    if (printem) message("Saving subset data object ", the.ld.name)
      
        #assign(envir = .frame0, inherits = TRUE, the.ld.name, data.subset)
    }
    return(data.subset)
}

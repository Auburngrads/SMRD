#' Title
#'
#' @param data.ld 
#' @param modes 
#' @param prefix 
#' @param allow.poor.data 
#' @param do.assign 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ShockAbsorber.ld <- frame.to.ld(shockabsorber,
#'                                 response.column = 1,
#'                                 failure.mode.column = 2,
#'                                 censor.column = 3, 
#'                                 time.units = "Kilometers")
#' summary(ShockAbsorber.ld)
#' event.plot(ShockAbsorber.ld)
#' 
#' # Split out by failure mode
#' 
#' mleprobplot(ShockAbsorber.ld, 
#'             distribution = "Weibull")
#' 
#' mfmi.mleprobplot(ShockAbsorber.ld, 
#'                  distribution = "Weibull")
#' 
#' mfmc.mleprobplot(ShockAbsorber.ld, 
#'                  distribution = "Weibull")
#'                  
#' ShockAbsorber.mfld <- mfm.to.ld(ShockAbsorber.ld)
#' 
#' multiple.mleprobplot(ShockAbsorber.mfld,
#'                      data.ld.name="xx",
#'                      xlab="yy",
#'                      distribution="Weibull")
#' 
#' mleprobplot(ShockAbsorber.Mode1.ld, 
#'             distribution = "Weibull")
#' 
#' mleprobplot(ShockAbsorber.Mode2.ld,
#'             distribution = "Weibull")
#' 
#' get.time.vector(ShockAbsorber.Mode2.ld)
#' }
mfm.to.ld <-
function (data.ld, 
          modes = NULL, 
          prefix = NULL, 
          allow.poor.data = T,
          do.assign = T)
{
    if (is.null(failure.modes(data.ld)))
        stop("Input must be a life data object with failure modes defined")
  
    if (is.null(prefix)) prefix <- GetDataPrefix(deparse(substitute(data.ld)))
    
    data.mfld <- ld.to.mfld(data.ld)
    
    if (is.null(modes)) modes <- names(data.mfld)
    
    for (i in 1:length(modes)) {
      
        the.ld.name <- paste(prefix, 
                             ".", 
                             strip.blanks.nulls(modes[i]),
                             ".ld", 
                             sep = "")
        
        poor.data <- !good.data(data.mfld[[modes[i]]], number.needed = 2)
        
        if (do.assign && (!poor.data || allow.poor.data)) {
          
            assign(envir = .frame0,
                   inherits = TRUE,
                   the.ld.name, 
                   data.mfld[[modes[i]]])
        }
        
        if (poor.data) {
            warning(paste("Fewer than two failures of type", modes[i]))
        }
    }
    return(data.mfld)
}

#' Render an enchapter help document
#'
#' @param chapter \code{numeric} Which chapter to run. Should be a value between 1 and 24
#' 
#' @importFrom rmarkdown render
#'
#' @export
echapter <- function(chapter){
  
  browseURL(rmarkdown::render(system.file("echapters",
                                          paste0("echapter",as.numeric(chapter),".Rmd"),
                                          package = "SMRD")))
  
}
#' Render an enchapter help document
#'
#' @param chapter \code{numeric} Which chapter to run. Should be a value between 1 and 24
#' 
#' @importFrom rmarkdown render
#' @import DT
#'
#' @export
echapter <- function(chapter){
  
  echapters = dir(system.file("echapters", package = "SMRD"), 
                  pattern = ".Rmd")
  
  echaps = gsub(".Rmd","", echapters)
  
  e_order = order(as.numeric(gsub("echapter","",echaps)))
  
  if(!isTRUE(paste0("echapter",as.numeric(chapter),".Rmd") %in% echapters)){
    
     stop("\n\nechapter",chapter,".Rmd does not exist -- Try one of the following:\n\n",
          paste(echapters[e_order], collapse = "\n"))
    
  }
  
  browseURL(rmarkdown::render(system.file("echapters",
                                          paste0("echapter",as.numeric(chapter),".Rmd"),
                                          package = "SMRD")))
  
}
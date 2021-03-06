get.subset.vector <-
function (subset, frame = NULL)
{
    if (is.logical(subset)) {
      
        `if`(length(subset) > 1,
             subset.name <- paste("(", deparse(substitute(subset)),")"),
             subset.name <- "")
        
  } else {
    
        if (!is.character(subset)) {
          
            stop(paste("subset must be either logical or a\ncharacter string with a logical component",
                paste(subset, collapse = ",")))
          
        }
    
        attach(frame, warn.conflicts = F)
        subset.name <- subset
        subset <- as.logical(ClistToVec(paste(paste(eval(parse(text = subset))),
            collapse = ",")))
        
        if (search()[2L] == "frame") detach(frame)
  }
  
    attr(subset, "subset.name") <- subset.name
    return(subset)
    
}

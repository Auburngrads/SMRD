data.object.type <-
function (data.ld)
{
    if ((is.data.frame(x = data.ld) || is.character(x = data.ld))) {
      
        if (!is.null(attr(data.ld, "response")) || !is.null(attr(data.ld, "response.column"))) return("frame.centered")
      
    }
  
    if (!any(is.na(match(c("frame", "response.column", "data.title"), names(data.ld))))) {
      
        if (is.onlist("life.data", oldClass(data.ld)))
            warning("List-centered data objects are deprecated---consider remaking")

        return("list.centered")
      
    }
  
    if (is.list(data.ld)) {
      
        if (is.onlist("life.data", oldClass(data.ld)) && (any(!is.na(match(c("y",
            "title"), names(data.ld)))) || any(!is.na(match(c("Response",
            "data.title"), names(data.ld))))))
            cat(paste("error problem Deprecated results object of an unfolded life data object-remake\n"))
      
        return(c("unfolded"))
    }
  
    print.default(data.ld)
    cat(paste("Corrupted data object", 
              paste(names(data.ld),
                    collapse = ",")), "\n")
    
    if (map.SMRDDebugLevel() >= 4) {
        cat("going intodebug1mode auto-browser\n")
        browser()
    }
    
    stop(paste("Corrupted data object", paste(names(data.ld),
        collapse = ",")))
}

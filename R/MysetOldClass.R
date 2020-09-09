MysetOldClass <-
function (Classes, where = 1)
{
    if (map.SMRDDebugLevel() == 0) {
        hold.warn <- options(warn = -1)
        on.exit(options(hold.warn))
    }
    if (map.SMRDDebugLevel() >= 4)
        cat("setting oldClasses", paste(Classes, collapse = ","), "\n")
  
    `if`(is.character(Classes),
         Classes <- as.character(Classes),
         stop(paste(c("expected a vector of strings for the class names: got class",
                      oldClass(Classes)),collapse = " ")))
    
    if (length(Classes) == 0) stop("no class names supplied")
    
    allOldClasses <- `if`(exists("oldClasses"),
                          get(envir = .frame0,  "oldClasses"),
                          character())
    
    allOldClasses <- sort(unique(c(Classes[[1]], allOldClasses)))
    
    assign(envir = .frame0, inherits = !TRUE, "oldClasses", allOldClasses)
    invisible()
}

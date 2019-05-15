SMRDOptions <-
function (..., save.options = FALSE)
{
    LocalSMRDOptionsDefaults <- SMRDOptionsDefaults()
   debug1<- FALSE

    if (nargs() == 0) {
        the.order <- order(names(LocalSMRDOptionsDefaults))
        return(LocalSMRDOptionsDefaults[the.order])
    }
    current <- LocalSMRDOptionsDefaults
    dotdotdot <- list(...)
    if (length(dotdotdot) == 1 && is.null(names(dotdotdot))) {
        arg <- dotdotdot[[1]]
        switch(mode(arg), list = {
            dotdotdot <- arg
        }, character = {
            the.return <- unlist(LocalSMRDOptionsDefaults[[arg]])
            names(the.return) <- arg
            return(the.return)
        }, {
            stop(paste("invalid argument:", arg))
        })
    }
    if (length(dotdotdot) == 0) return()
    the.names <- names(dotdotdot)
    if (is.null(the.names)) stop("options must be given by name")
    changed <- current[the.names]
    names(changed) <- the.names
    names(dotdotdot) <- NULL
    for (i in 1:length(the.names)) {
        current[[the.names[i]]] <- dotdotdot[[i]]
    }
    if (debug1) {
        cat("browser at the end\n")
        browser()
    }
    invisible(changed)
}

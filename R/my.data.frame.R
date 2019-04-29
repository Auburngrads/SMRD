my.data.frame <-
  function (...) 
  {
    call <- match.call()
    m <- match.call(expand = FALSE)

      the.args <- m[[2]]
      not.null <- rep(TRUE, length(the.args))
      the.parent.frame <- parent.frame()
      
      for (i in 1:length(the.args)) {
        the.values <- get(as.character(the.args[[i]]), envir = the.parent.frame)
        if (is.null(the.values)) 
          not.null[i + 1] <- FALSE
      }
   
    call <- call[not.null]
    call[[1]] <- as.name("data.frame")
    eval(call, sys.parent())
  }

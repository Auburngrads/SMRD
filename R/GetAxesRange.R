GetAxesRange <-
function (type, x.axis, xlim, xlab, y.axis, ylim, ylab)
{
    recovered <- F

SetAxesRange <- function (type, x.axis, xlim, xlab, y.axis, ylim, ylab)
      {
        switch(type, probplot.setup = {
          file.name <- ".axes.probplot.setup"
        }, event.plot.setup = {
          file.name <- ".axes.event.plot.setup"
        }, plot.paper = {
          file.name <- ".axes.plot.paper"
        }, {
          stop(paste(type, "is not recognized\n"))
        })
        if (exists(file.name)) {
          the.list <- get(envir = .frame0, file.name)
      } else the.list <- NULL
        the.axes.name <- casefold(paste(x.axis, xlab, y.axis, ylab,
                                        sep = ""))
        the.list[[the.axes.name]] <- list(xlim = xlim, x.axis = x.axis,
                                          xlab = xlab, ylim = ylim, y.axis = y.axis, ylab = ylab)
        assign(envir = .frame0,  inherits = TRUE,file.name, the.list)
        invisible()
      }

is.LockAxes <- function ()
      {
        if (exists(".LockAxes")) {
          LockAxes <- get(envir = .frame0,  ".LockAxes")
          if (LockAxes)
            return(T)
        }
        return(F)
      }

is.AxesAgree <- function (file.name, x.axis, xlim, xlab, y.axis, ylim,
            ylab)
  {
    if (exists(file.name)) {
      the.axes.list <- get(envir = .frame0, file.name)
      the.axes.name <- casefold(paste(x.axis, xlab, y.axis,
                                      ylab, sep = ""))
      the.axes <- the.axes.list[[the.axes.name]]
      if (is.null(the.axes))
        return(F)
      newx.bigger <- xlim[1] > the.axes$xlim[2]
      newx.smaller <- xlim[2] < the.axes$xlim[1]
      newy.bigger <- ylim[1] > the.axes$ylim[2]
      newy.smaller <- ylim[2] < the.axes$ylim[1]
      if (newx.bigger || newx.smaller || newy.bigger || newy.smaller)
        return(F)
      else return(T)
    } else return(T)
  }

        switch(type, probplot.setup = {
        file.name <- ".axes.probplot.setup"
    }, event.plot.setup = {
        file.name <- ".axes.event.plot.setup"
    }, plot.paper = {
        file.name <- ".axes.plot.paper"
    }, {
        stop(paste(type, "is not recognized\n"))
    })
    if (is.LockAxes() && exists(file.name) &&
        is.AxesAgree(file.name, x.axis, xlim, xlab, y.axis,
            ylim, ylab)) {
        the.axes.list <- get(envir = .frame0, file.name)
        the.axes.name <- casefold(paste(x.axis, xlab, y.axis,
            ylab, sep = ""))
        the.axes <- the.axes.list[[the.axes.name]]
        recovered <- T
        cat("****Using a saved SMRD axes.****\n")
  } else the.axes <- list(xlim = xlim, x.axis = x.axis,
        xlab = xlab, ylim = ylim, y.axis = y.axis, ylab = ylab)
    SetAxesRange(type, the.axes$x.axis, the.axes$xlim, the.axes$xlab,
        the.axes$y.axis, the.axes$ylim, the.axes$ylab)
    if (map.SMRDDebugLevel() >= 4) {
        cat("In GetAxesRange type=", type, "Recovered=", recovered,
            "\n")
        print(the.axes)
    }
    return(the.axes)
}

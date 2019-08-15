model.plot.paper <-
function (xlim = c(NA, NA), ylim = c(NA, NA), x.axis = "linear", 
    y.axis = "linear", grids = T, title.option = "blank", my.title = NULL, 
    ylab = "", xlab = "", response.on.yaxis = T, original.par = F) 
{
    old.par <- par()
    par(mar = c(5, 6, 4, 2) + 0.1, err = -1)
    if (original.par) 
        on.exit(par(old.par))
    hold.x.axis <- x.axis
    if (generic.relationship.name(x.axis) == "Arrhenius") {
        x.axis <- "Arrhenius3"
        attr(x.axis, "the.arrhenius.units") <- attr(hold.x.axis, 
            "the.arrhenius.units")
    }
    switch(generic.relationship.name(hold.x.axis), squareroot = , 
        linear = {
            xlim.default <- c(0, 10)
        }, reciprocal = , `Box-Cox` = , log = {
            xlim.default <- c(1, 10)
        }, logit = {
            xlim.default <- c(0.01, 0.99)
        }, , humidity = {
            xlim.default <- c(1, 99)
        }, Eyring = , Arrhenius = {
            xlim.default <- c(20, 200)
            if (xlab == "") xlab <- "Degrees C"
        })
    ylim.default <- switch(generic.relationship.name(y.axis), 
        squareroot = , linear = {
            c(0, 10)
        }, log = {
            c(1, 10)
        }, Eyring = , Arrhenius = {
            c(20, 200)
        })
    xrna <- is.na(xlim)
    if (any(xrna)) 
        xlim[xrna] <- xlim.default[xrna]
    yrna <- is.na(ylim)
    if (any(yrna)) 
        ylim[yrna] <- ylim.default[yrna]
    if (generic.relationship.name(x.axis) == "Box-Cox") {
        the.power <- attr(x.axis, "the.power")
        x.axis.name <- paste(x.axis, "(", the.power, ")", sep = "")
    }
    else {
        x.axis.name <- hold.x.axis
    }
    if (is.null(my.title)) 
        my.title <- paste(y.axis, "vs", x.axis.name)
    if (is.null(my.title)) 
        my.title <- paste(x.axis.name, "vs", y.axis)
    plot.paper(x = ylim, y = xlim, x.axis = y.axis, 
        y.axis = x.axis, , ylab = xlab, xlab = ylab, response.on.yaxis = response.on.yaxis, 
        grids = grids, title.option = title.option, my.title = my.title, 
        cex.title = 1)
    invisible()
}

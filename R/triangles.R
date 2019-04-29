triangles <-
function (x, y, orient, size = 0.1, ...) 
{
    ydelta <- y.loc(1) - y.loc(0)
    xdelta <- x.loc(1) - x.loc(0)
    aspect.ratio <- ydelta/xdelta
    size.factor <- size * xdelta * 0.3
    std.points <- function(orient) {
        sqr3d3 <- sqrt(3)/3
        oned3 <- 1/3
        twod3 <- 2/3
        switch(orient, up = {
            x <- c(-sqr3d3, sqr3d3, 0, -sqr3d3)
            y <- c(oned3, oned3, twod3, oned3)
        }, down = {
            x <- c(sqr3d3, -sqr3d3, 0, sqr3d3)
            y <- c(-oned3, -oned3, -twod3, -oned3)
        }, left = {
            x <- c(-oned3, -oned3, -twod3, -oned3)
            y <- c(sqr3d3, -sqr3d3, 0, sqr3d3)
        }, right = {
            x <- c(oned3, oned3, twod3, oned3)
            y <- aspect.ratio * c(-sqr3d3, sqr3d3, 0, -sqr3d3)
        }, {
            stop(paste(orient[i], "orient not recognized"))
        })
        return.xy <- cbind(x = x, y = y)
        return.xy
    }
    for (i in 1:length(x)) {
        if (length(orient) == 1) 
            the.orient <- size.factor * std.points(orient)
        else the.orient <- size.factor * std.points(orient[i])
        lines(x[i] + the.orient[, "x"], y[i] + the.orient[, "y"], 
            ...)
    }
}

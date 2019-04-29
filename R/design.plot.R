design.plot <-
function (data.ld, x.cols = c(1, 2), xlab = get.x.columns(data.ld)[x.cols[1]], 
    ylab = get.x.columns(data.ld)[x.cols[2]], pch = 16, cex = 1.5) 
{
    vec1 <- xmat(data.ld)[, x.cols[1]]
    vec2 <- xmat(data.ld)[, x.cols[2]]
    plot.paper(range(vec1), range(vec2), grids = F, xlab = xlab, 
        ylab = ylab, cex = 1.5)
    points.default(vec1, vec2, pch = pch, cex = cex)
}

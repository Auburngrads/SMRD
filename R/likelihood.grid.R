likelihood.grid <-
function (gmle.out, range.list, log.quantile = F, the.quantile = NA,
    size = 21, relative = F, yname = "yname", xname = "xname",
   debug1= -1, monitor = 1, save.it = F)
{
    is.min <- function (x) { x == min(x) }
    distribution <- gmle.out$model$distribution
    relfun <- gmle.out$log.like
    assign.gmle(gmle.out, debug1, monitor)
    param.matrix <- get.seq.mat(range.list, size = size)
    if (generic.distribution(distribution) == "weibull") {
        param.matrix[, 2] <- 1/param.matrix[, 2]
    }
    if (!is.na(the.quantile)) {
        zquant <- quant(the.quantile, distribution)
        if (!log.quantile && is.logdist(distribution)) {
            param.matrix[, 1] <- logb(param.matrix[, 1]) - zquant *
                param.matrix[, 2]
      } else {
            param.matrix[, 1] <- param.matrix[, 1] - zquant *
                param.matrix[, 2]
        }
    }
    likevec <- -relfun(param.matrix)
    loc.max <- (1:length(likevec))[is.min(likevec)]
    if (relative) {
        likevec <- exp((likevec - max(likevec)))
        likevec[likevec < 1e-06] <- 0
    }
    thetavec1 <- seq(range.list[[1]][1], range.list[[1]][2],
        length = size)
    thetavec2 <- seq(range.list[[2]][1], range.list[[2]][2],
        length = size)
    the.list <- list(x = thetavec1, y = thetavec2, z = matrix(likevec,
        nrow = size, byrow = T), number.parameters = 2, subtitle = get.data.title(gmle.out$data.ld),
        xlab = xname, ylab = yname, distribution = distribution,
        log.quantile = log.quantile)
    grid.name <- paste(deparse(substitute(gmle.out)), ".grid",
        sep = "")
    if (save.it)
        assign(envir = .frame0,  inherits = TRUE,grid.name, the.list)
    invisible(the.list)
}

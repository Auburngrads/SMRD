mcf.segments <-
function (time, rlevel, maxtime, lty, lwd, col = 1) 
{
    endtime <- length(time)
    time <- c(0, time, maxtime)
    rlevel <- c(0, rlevel, rlevel[endtime])
    s <- seq(length(time - 1))
    segments(time[s], rlevel[s], time[s + 1], rlevel, lty = lty, 
        lwd = lwd, col = col)
}

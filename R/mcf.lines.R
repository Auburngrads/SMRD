mcf.lines <-
function (time, rlevel, maxtime, lty, lwd = lwd, col = 1) 
{
    time <- c(0, time)
    rlevel <- c(0, rlevel)
    endtime <- length(time)
    lines(time, rlevel, lty = lty, lwd = 3, col = col)
    segments(time[endtime], rlevel[endtime], maxtime, rlevel[endtime], 
        lty = lty, lwd = lwd, col = col)
}

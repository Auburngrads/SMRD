lev.sim <-
function () 
{
    for (i in 1:10) {
        lev.sample <- qlev(runif(500))
        plot(sort(lev.sample), qlev(((1:length(lev.sample)) - 
            0.5)/length(lev.sample)))
        pause()
    }
}

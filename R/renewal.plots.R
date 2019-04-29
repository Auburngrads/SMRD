renewal.plots <-
function (data.rdu, my.title = NULL) 
{
    repair.tsplot(data.rdu)
    pause()
    interarrival.plot(data.rdu)
    pause()
    ar1.plot(data.rdu)
    invisible()
}

`data.title<-` <-
function (data.ld, value) 
{
    attr(data.ld, "data.title") <- value
    return(data.ld)
}

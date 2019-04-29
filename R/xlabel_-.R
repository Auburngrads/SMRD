`xlabel<-` <-
function (data.ld, value) 
{
    attr(data.ld, "xlabel") <- value
    return(data.ld)
}

pp.data <-
function (data.vector, log.of.data) 
{
    `if`(log.of.data,
         return(logb(data.vector)),
         return(data.vector))
}

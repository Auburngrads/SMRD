get.data <-
function (results) 
{
    data.rmd <- attr(results, "data.rmd")
    return(data.rmd)
}

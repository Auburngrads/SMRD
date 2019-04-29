get.rmd.subset <-
function (data.rmd, right.stuff) 
{
    data.rmd <- data.rmd[right.stuff, ]
    return(data.rmd)
}

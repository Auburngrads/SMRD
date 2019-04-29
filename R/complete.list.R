complete.list <-
function (data.d) 
{
    return(attr(data.d, "complete.list"))
}

#'
#'

`complete.list<-` <-
  function (data.ld, value) 
  {
    attr(data.ld, "complete.list") <- value
    return(data.ld)
  }


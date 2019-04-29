done.list <-
function (data.d)
{
    return(attr(data.d, "done.list"))
}

#'
#'

`done.list<-` <-
  function (data.ld, value)
  {
    attr(data.ld, "done.list") <- value
    return(data.ld)
  }

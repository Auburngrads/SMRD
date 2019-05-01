stress.order <-
function (data.d) 
{
    return(attr(data.d, "stress.order"))
}

#
#

`stress.order<-` <-
  function (data.ld, value) 
  {
    attr(data.ld, "stress.order") <- value
    return(data.ld)
  }

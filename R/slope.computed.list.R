slope.computed.list <-
function (data.d) 
{
    return(attr(data.d, "slope.computed.list"))
}

#
#

`slope.computed.list<-` <-
  function (data.ld, value) 
  {
    attr(data.ld, "slope.computed.list") <- value
    return(data.ld)
  }

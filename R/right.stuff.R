right.stuff <-
function (data.ld)
{
    return(attr(data.ld, "right.stuff"))
}

#
#

`right.stuff<-` <-
  function (data.ld, value)
  {
    attr(data.ld, "right.stuff") <- value
    return(data.ld)
  }

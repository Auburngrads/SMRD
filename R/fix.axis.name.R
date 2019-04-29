fix.axis.name <-
function (the.axis) 
{
    if (length(the.axis) > 1) 
        warning("length of the axis in greater than 1")
    switch(generic.relationship.name(the.axis[1]), Arrhenius3 = {
        the.axis[1] <- "Arrhenius"
    }, reciprocal3 = {
        the.axis[1] <- "reciprocal"
    }, invtemp3 = {
        the.axis[1] <- "invtemp"
    }, {
    })
    return(the.axis)
}

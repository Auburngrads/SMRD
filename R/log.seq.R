log.seq <-
function (start, finish, length)
{
    return(exp(seq(logb(start), logb(finish), length = length)))
}

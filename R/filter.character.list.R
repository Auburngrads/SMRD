filter.character.list <-
function (the.base.list, the.sub.list, strip = T) 
{
    if (strip) 
        return(the.base.list[is.na(match(the.base.list, the.sub.list))])
    else return(the.base.list[!is.na(match(the.base.list, the.sub.list))])
}

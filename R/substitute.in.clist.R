substitute.in.clist <-
function (clist, find.string, replace.string, sep = ",") 
{
    Vec <- ClistToVec(clist, sep = sep)
    matches <- Vec == find.string
    if (any(matches)) {
        Vec[matches] <- replace.string
        return(paste(Vec, collapse = sep))
    }
    else return(clist)
}

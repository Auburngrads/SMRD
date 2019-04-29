extract.results <-
function (groupm.list) 
{
    if (is.onlist("mlest", oldClass(groupm.list[[1]]))) {
        groupm.out <- groupm.list[[1]]
    }
    else {
        groupm.out <- groupm.list
    }
    return(groupm.out)
}

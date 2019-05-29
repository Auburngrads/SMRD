levels.groupm.out <-
function (x)
{
    
    `if`(!is.onlist("life.data", oldClass(x[[1]])),
         groupm.out <- x[[1]],
         groupm.out <- x)
    
    stresses <- get.x.markers(groupm.out$data.ld, 
                              groupm.out$group.var)
    
    return(stresses)
}

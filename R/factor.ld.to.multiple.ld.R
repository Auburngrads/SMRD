factor.ld.to.multiple.ld <-
function (data.ld, 
          group.var = 1, 
          stresses = get.x.markers(data.ld,
                                    group.var = group.var, 
                                    do.order = T))
{
    stress.names <- get.x.markers(data.ld, 
                                   group.var = group.var,
                                   long = T, 
                                   do.order = T)
    data.list <- list()
    
    for (i in 1:length(stresses)) {
      
         sub.data.ld <- multiple.get.data.subset(data.ld, 
                                                 stresses[i],
                                                 columns = group.var)
         
         data.list[[stress.names[i]]] <- sub.data.ld
    }
    
    oldClass(data.list) <- "multiple.life.data"
    attr(data.list, "data.ld") <- data.ld
    return(data.list)
}

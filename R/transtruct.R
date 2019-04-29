transtruct <-
function (struct, transformation, ...)
{
    if (is.function(transformation)) {
      
        tmp <- transformation(struct$x, ...)
        struct$x <- tmp
        
    } else {
      
        struct$x <- f.relationshipinv(struct$x, transformation)
    }
  
    return(struct)
}

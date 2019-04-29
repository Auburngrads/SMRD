frame.new.data <-
function (the.string, groupm.out)
{
    tmp.frame <- string.to.frame(the.string)
    tmp.x.names <- colnames(xmat(groupm.out$the.string$data.ld))
    group.var <- groupm.out$group.var
    
    if (length(tmp.x.names) == ncol(tmp.frame)) {
      
        the.x.names <- tmp.x.names
        
    } else {
      
        if (length(group.var) == ncol(tmp.frame)) {
            the.x.names <- tmp.x.names[group.var]
            
      } else {
        
            print(tmp.frame)
            stop(paste("\nBad new.data string---number of variables does not agree:",
                the.string, "\ngroup.var=", paste(group.var,
                  collapse = ","), "\n"))
        }
    }
    string.to.frame(the.string, col.names = the.x.names)
}

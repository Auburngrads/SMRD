insert.class.xlevels <-
function (the.new.subframe, the.xmat, group.var = 1:ncol(the.xmat)) 
{
    if (length(group.var) > 0) {
        for (i in 1:length(group.var)) {
            if (is.factor(the.xmat[[group.var[i]]])) {
                the.new.subframe[[group.var[i]]] <- factor(the.new.subframe[[group.var[i]]], 
                  levels = levels(the.xmat[[group.var[i]]]))
            }
        }
    }
    return(the.new.subframe)
}

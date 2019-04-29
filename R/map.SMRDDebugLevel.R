map.SMRDDebugLevel <-
function (the.level = SMRDOptions("SMRD.DebugLevel")) 
{
    result <- switch(the.level, off = , weak = , none = {
        0
    }, low = {
        1
    }, moderate = {
        2
    }, development = {
        4
    }, developmentplus = {
        6
    }, high = {
        8
    }, full = {
        10
    }, browser = {
        101
    })
    names(result) <- the.level
    return(result)
}

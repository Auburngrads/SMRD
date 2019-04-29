is.valid.variable.name <-
function (string) 
{
    parsed.string <- cparse(string)
    the.match <- match(parsed.string, 
                       c(as.character(0:9), 
                         letters, 
                         LETTERS, ".", " "))
    
    if (!any(is.na(the.match))) {
      
        return(T)
      
    } else {
      
        answer <- F
        attr(answer, "bad.char") <- parsed.string[is.na(the.match)]
        return(answer)
    }
}

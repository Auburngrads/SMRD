ask.if <-
function (prompt = "Continue? ") 
{
    if (!interactive() || (exists(".Batch") && .Batch)) 
        return(TRUE)
    repeat {
        cat(prompt)
        ans <- readline()
        invisible()
        if (ans == "y" | ans == "yes") 
            return(TRUE)
        if (ans == "n" | ans == "no") 
            return(FALSE)
        cat("Answer y or n\n")
    }
}

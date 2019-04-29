newpause <-
function (prompt = "Touch Return to continue; type n to breakout s to save: ") 
{
    repeat {
        cat(prompt)
        ans <- readline()
        invisible()
        if (ans == "n" | ans == "no") 
            return("n")
        if (ans == "") 
            return("")
        if (ans == "s" | ans == "ss") 
            return("s")
    }
}

y.loc <-
function (perc.loc) 
{
    usr.out <- par("usr")
    return(usr.out[3] + perc.loc * (usr.out[4] - usr.out[3]))
}

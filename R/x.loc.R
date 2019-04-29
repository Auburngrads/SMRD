x.loc <-
function (perc.loc) 
{
    usr.out <- par("usr")
    return(usr.out[1] + perc.loc * (usr.out[2] - usr.out[1]))
}

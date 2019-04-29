f.getvec <-
function (nread = -1) 
{
    if (nread == -1) 
        return(scan())
    else return(scan(n = nread))
}

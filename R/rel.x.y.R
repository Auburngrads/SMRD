rel.x.y <-
function (xpos, ypos) 
{
    usr.out <- par("usr")
    x.pos <- usr.out[1] + xpos * (usr.out[2] - usr.out[1])
    y.pos <- usr.out[3] + ypos * (usr.out[4] - usr.out[3])
    return(list(x = x.pos, y = y.pos))
}

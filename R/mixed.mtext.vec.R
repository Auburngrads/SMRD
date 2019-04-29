mixed.mtext.vec <-
function (side, at, texts, adj, cex, line, srt = 0) 
{
    for (i in seq(along = at)) {
        mixed.mtext(side = side, at = at[i], texts = texts[i], 
            adj = adj, cex = cex, line = line, srt = srt)
    }
}

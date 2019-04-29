vector.strip.blanks <-
function (x, StripChar = " ", FillChar = " ")
{
    for (i in 1:length(x)) {
        x[i] <- my.strip.blanks(x[i], StripChar = StripChar,
            FillChar = FillChar)
    }
    x
}

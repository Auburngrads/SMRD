my.write <-
function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5,
    append = FALSE, sep = " ")
cat(x, sep = c(rep(sep, ncolumns - 1), "\n"), file = file, append = append)

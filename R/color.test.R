color.test <-
function (n = 16) 
{
    if (!is.R()) {
        pie(rep(1, length = n + 1), col = 0:(n), names = as.character(0:n))
    }
    else {
        pie(rep(1, length = n + 1), col = 0:(n), labels = as.character(0:n))
    }
}

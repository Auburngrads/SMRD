ls.mode <-
function (funname = NULL, where = 1, mode = "function", all.names = FALSE)
{
    dimnames(wqm.objects.summary(mode = mode, where = where,
            all.names = all.names))[[1]]
  }

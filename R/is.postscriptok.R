is.postscriptok <-
function () 
{
    `if`(names(dev.cur()) == "postscript",
         postsctiptok <- T,
         postsctiptok <- F)

      return(postsctiptok)
}


is.postsctiptok <-
function () 
{
    `if`(names(dev.cur()) == "postscript" && exists("unix"),
         postsctiptok <- T,
         postsctiptok <- F)

      return(postsctiptok)
}
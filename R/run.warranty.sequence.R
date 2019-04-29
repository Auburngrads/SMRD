run.warranty.sequence <-
function (Lcode, path = "/home/wqmeeker/projects/warranty/data/",
    max.number = NULL)
{
    path2 <- "/home/wqmeeker/rje/kwentzla/"
    raw.warranty <- read.table(paste(path, Lcode, ".dat", sep = ""),
        header = T)
    bad.data <- raw.warranty$K.Miles <= 0 | raw.warranty$Months <=
        0
    warranty <- raw.warranty[!bad.data, ]
    warranty.sequence(warranty, max.number = max.number)
}

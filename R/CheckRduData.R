CheckRduData <-
function (frame) 
{
    Unit.ID <- frame$Unit.ID
    uniqueUnit.ID <- unique(Unit.ID)
    Status <- as.character(frame$Status)
    for (i in 1:length(uniqueUnit.ID)) {
        the.ones <- uniqueUnit.ID[i] == Unit.ID
        sub.frame <- frame[the.ones, ]
        if (nrow(sub.frame) == 1) 
            next
        Model <- sub.frame$Model
        uniqueModel <- unique(Model)
        if (length(uniqueModel) > 1) {
            print(sub.frame[, c(1, 3, 5, 6)])
            row.numbers <- dimnames(sub.frame)[[1]]
            subStatus <- as.character(sub.frame$Status)
            the.end.one <- subStatus == "End"
            uniqueNotEnd <- unique(Model[!the.end.one])
            if (length(uniqueNotEnd) == 1) {
                end.number <- as.numeric(row.numbers[the.end.one])
                cat("Corrected\n")
                print(frame[the.ones, c(1, 3, 5, 6)])
            }
            else {
                endModel <- Model[the.end.one]
                end.number <- as.numeric(row.numbers[the.end.one])
                cat("Corrected********************\n")
                print(frame[the.ones, c(1, 3, 5, 6)])
            }
        }
    }
    Cost <- rep(NA, ncol(frame))
    Cost[Status == "End"] <- 0
    Cost[Status == "Fail"] <- 1
    frame <- data.frame(frame, Cost = Cost)
    frame
}

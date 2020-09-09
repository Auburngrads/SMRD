MakeMultipleRdu <-
function (frame.names, ID.column, time.column, event.column)
{
    for (i in 1:length(frame.names)) {
        frame.rdu <- frame.to.rdu(frame = get(envir = .frame0, frame.names[i]),
            ID.column = ID.column, time.column = time.column,
            event.column = event.column, data.title = paste(frame.names[i],
                "Data"))
        file.name <- paste(frame.names[i], "rdu", sep = ".")
        cat("Saving subset", file.name, "\n")
        assign(envir = .frame0, inherits = !TRUE,file.name, value = frame.rdu)
    }
}

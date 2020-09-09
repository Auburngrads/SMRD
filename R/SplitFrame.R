SplitFrame <-
function (frame, split.variable, name.prefix = deparse(substitute(frame)),
    sepChars = split.variable)
{
    SubFrameList <- split(frame, frame[[split.variable]])
    SubFrameListNames <- names(SubFrameList)
    for (i in 1:length(SubFrameList)) {
        file.name <- paste(name.prefix, SubFrameListNames[i],
            sep = sepChars)
        cat("Saving subset", file.name, "\n")
        assign(envir = .frame0, inherits = !TRUE,file.name, value = SubFrameList[[i]])
    }
    namesOfObjects <- paste(name.prefix, SubFrameListNames, sep = sepChars)
    invisible(namesOfObjects)
}

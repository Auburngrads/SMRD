multiple.FortranDebug <-
function (NAME, ..., NAOK = F, specialsok = F, pointers = NULL,
    data.file.name = "fort_data_file.txt", fort.file.name = "fort_code_file.f",
    ncalls = 1, icall = 1)
{
    m <- match.call(expand.dots = F)
    subroutine.name <- m[[2]]
    the.data.file.name <- paste(data.file.name, icall, sep = "")
    arg.list <- eval(m[[3]], envir = .frame0)
    type.list <- list()
    call.vec <- rep(" ", length(arg.list))
    call.string <- rep(" ", length(arg.list))
    write(paste("c      fortran debugger from .FortranDebug",
        date()), file = fort.file.name)
    write(ncalls, file = the.data.file.name)
    write(paste("      integer ncalls(1)"), file = fort.file.name,
        append = T)
    for (i in 1:length(arg.list)) {
        the.arg.now <- arg.list[[i]]
        the.length <- length(the.arg.now)
        the.mode <- storage.mode(the.arg.now)
        write(the.length, file = the.data.file.name, append = T)
        write(the.arg.now, file = the.data.file.name, append = T)
        switch(the.mode, integer = {
            type.string <- "      integer "
            call.string[i] <- "i"
        }, single = {
            type.string <- "      single precision "
            call.string[i] <- "r"
        }, double = {
            type.string <- "      double precision "
            call.string[i] <- "d"
        }, {
            stop(paste("unrecognized storage mode", the.mode))
        })
        write(paste(type.string, call.string[i], i, "(", the.length,
            ")", sep = ""), file = fort.file.name, append = T)
        call.vec[i] <- paste(call.string[i], i, sep = "")
    }
    for (i in 1:length(arg.list)) {
        if (i == 1) {
            write("      read(5,*) ncalls", file = fort.file.name,
                append = T)
            write("      do 22 i=1,ncalls", file = fort.file.name,
                append = T)
            write("      rfile=rf(i)", file = fort.file.name,
                append = T)
            write("      open(rfile,5)", file = fort.file.name,
                append = T)
        }
        write(paste("         read(5,*)", paste(call.string[i],
            i, sep = "")), file = fort.file.name, append = T)
    }
    the.call <- paste("      call", subroutine.name, "(", paste(call.vec,
        collapse = ","), ")")
    write(the.call, file = fort.file.name, append = T)
    write("22    continue", file = fort.file.name, append = T)
    write("      stop", file = fort.file.name, append = T)
    write("      end", file = fort.file.name, append = T)
    .Fortran(NAME, ..., NAOK = F, specialsok = F, pointers = NULL)
}

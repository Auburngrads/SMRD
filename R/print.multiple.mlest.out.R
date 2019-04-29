print.multiple.mlest.out <-
function (x, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
    digits = GetSMRDDefault("SMRD.DigitsPrinted"), print.vcv = GetSMRDDefault("SMRD.LongPrint"),
    quote = T, prefix = "", add.title.in = NULL,...)
{
    for (i in 1:length(x)) {
        if (!any(is.na(x[[i]]))) {
            if (is.null(add.title.in))
                add.title <- paste(" Group", names(x)[i])
            else add.title <- add.title.in
            print(x[[i]], conf.level = conf.level,
                digits = digits, print.vcv = print.vcv, add.title = add.title,
                quote = quote, prefix = prefix)
        }
    }
    invisible()
}

summary.simulate.ADDT.out <-
function (object,...)
{
    cat(attr(object, "plan.string"), "\n")
    cat(attr(object, "plan.values.string"), "\n")
    print(attr(object, "plan.values"))
    cat("Contains ", nrow(object), " trials\n", sep = "")
}

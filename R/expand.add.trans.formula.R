expand.add.trans.formula <-
function (old.formula, variables = unique(unlist(unpasted)),
    relationship = rep("Linear", length = length(variables)))
{
    parse.formula <- vector.strip.blanks(as.character(old.formula))
    if (parse.formula == "Location~")
        stop("Empty formula")
    curr.attr <- terms(as.formula(old.formula))
    the.terms <- attr(curr.attr, "term.labels")
    unpasted <- lapply(the.terms, wqm.unpaste, sep = ":")
    if (length(variables) != length(relationship))
        stop(paste("length(variables)=", length(variables),
            "not equal to length(relationship)=", length(relationship)))
    unlisted <- unlist(unpasted)

        w1 <- regexpr("^I\\(*", unlisted)
        w2 <- regexpr("\\^.\\)$", unlisted)

    unique.unlisted <- unique(ifelse(w1 < 0, unlisted, substring(unlisted,
        w1 + attr(w1, "match.length"), w2 - 1)))
    check.unlisted <- is.na(match(unique.unlisted, variables))
    if (any(check.unlisted)) {
        stop(paste("\n    Undefined variables detected in formula:",
            paste(unlisted[(1:length(unique.unlisted))[check.unlisted]])))
    }
    the.exp.var <- NULL
    for (i in 1:length(unpasted)) {
        current.terms <- unlist(unpasted[[i]])

            w1 <- regexpr("^I\\(*", current.terms)
            w2 <- regexpr("\\^.\\)$", current.terms)

        stripped.current.terms <- ifelse(w1 < 0, current.terms,
            substring(current.terms, w1 + attr(w1, "match.length"),
                w2 - 1))
        I.stuff <- ifelse(w1 < 0, "", "I(")
        power.stuff <- ifelse(w1 < 0, "", substring(current.terms,
            w2, w2 + attr(w2, "match.length") - 1))
        the.relationships <- relationship[match(current.terms,
            variables)]
        trivial <- the.relationships == "Class" | the.relationships ==
            "Linear"
        beginning <- ifelse(trivial, "", "g(")
        ending <- ifelse(trivial, "", ")")
        new.terms <- paste(I.stuff, beginning, stripped.current.terms,
            ending, power.stuff, collapse = ":", sep = "")
        the.exp.var <- paste(the.exp.var, new.terms, sep = "+",
            collapse = "")
    }
    the.formula <- as.formula(paste("Location", the.exp.var,
        sep = "~"))
    return(the.formula)
}

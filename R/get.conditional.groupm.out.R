get.conditional.groupm.out <-
function (focus.variable, fixed.other.values, groupm.out)
{
  CheckString <- function (pat, str) { return(regexpr(pat, str) > 0) }

    conditional.groupm.out <- groupm.out
    variable.names <- colnames(xmat(groupm.out$data.ld))
    if (is.numeric(focus.variable)) {
        focus.variable.index <- focus.variable
        focus.variable <- variable.names[focus.variable.index]
  } else focus.variable.index <- match(focus.variable, variable.names)
    focus.variable.relationship <- subscript.relationship(groupm.out$relationship,
        focus.variable.index)
    assign(envir = .frame0, inherits = !TRUE,"relationship.vector", groupm.out$relationship)
    the.terms <- groupm.out$terms
    term.labels <- attr(the.terms, "term.labels")
    class.focus <- focus.variable.relationship == "Class" ||
        focus.variable.relationship == "class"
    if (class.focus) {
        insert.for.focus.out <- insert.levels.for.focus(fixed.other.values,
            focus.variable.index, xmat(groupm.out$data.ld))
  } else {
        insert.for.focus.out <- insert.one.for.focus(fixed.other.values,
            focus.variable.index, focus.variable.relationship)
    }
    fixed.other.values.frame <- string.to.frame(insert.for.focus.out,
        col.names = variable.names)
    focus.variable.model.matrix <- model.matrix(delete.response(the.terms),
        data = fixed.other.values.frame, contrasts = the.terms$contrasts,
        xlevels = get.class.xlevels(xmat(groupm.out$data.ld),
            the.terms))
    expanded.term.labels <- dimnames(focus.variable.model.matrix)[[2]]
    if (class.focus) {
        focus.variable.model.matrix <- t(as.matrix(apply(focus.variable.model.matrix,
            2, max)))
    }
    power.vec <- rep(NA, length(expanded.term.labels))
    for (i in 1:length(expanded.term.labels)) {
        current.factors <- unlist(lapply(expanded.term.labels[i],
            wqm.unpaste, sep = ":"))
        focus.included <- CheckString(focus.variable, current.factors)
        attributes(focus.included) <- NULL
        power.vec[i] <- 0
        if (any(focus.included)) {
            the.focus.term <- current.factors[focus.included]
            if (length(the.focus.term) > 1)
                stop(paste("Term", "has more than one exponent (power) of the focus variable",
                  expanded.term.labels[i]))
            w2 <- regexpr("\\^", the.focus.term)
            if (w2 > 0)
                power.vec[i] <- as.integer(substring(current.factors,
                  w2 + 1, w2 + attr(w2, "match.length")))
            else power.vec[i] <- 1
        }
    }
    include.intercept <- attr(the.terms, "intercept") == 1
    if (include.intercept) {
        conditional.names <- "(Intercept)"
  } else {
        conditional.names <- NULL
    }
    unique.power <- sort(unique(power.vec))
    unique.positive.power <- unique.power[unique.power > 0]
    power.stuff <- ifelse(unique.positive.power < 2, "", paste("^",
        unique.positive.power, sep = ""))
    if (include.intercept || any(unique.power == 0)) {
        no.intercept <- ""
  } else {
        no.intercept <- "-1"
    }
    new.formula <- paste("Location~", no.intercept, paste(focus.variable,
        power.stuff, sep = "", collapse = "+"))
    new.formula <- expand.add.trans.formula(new.formula, variables = focus.variable,
        relationship = focus.variable.relationship)
    new.terms <- terms(as.formula(new.formula))
    the.names <- dimnames(focus.variable.model.matrix)[[2]]
    test.for.focus.only <- matrix(unlist(lapply(focus.variable,
        regexpr, the.names)) > 0, ncol = length(the.names), byrow = T)
    test.for.non.focus <- matrix(unlist(lapply(variable.names[-focus.variable.index],
        regexpr, the.names)) < 0, ncol = length(the.names), byrow = T)
    test.for.focus.only <- apply(rbind(test.for.focus.only, test.for.non.focus),
        2, all)
    conditional.names <- c(conditional.names, the.names[test.for.focus.only],
        "sigma")
    focus.term.occurrence <- matrix(unlist(lapply(the.names[test.for.focus.only],
        regexpr, the.names)) > 0, ncol = length(the.names), byrow = T)
    focus.term.occurrence <- rbind((!apply(focus.term.occurrence,
        2, any)), focus.term.occurrence, rep(F, length = length(the.names)))
    if (map.SMRDDebugLevel() > 3) {
        print(test.for.focus.only)
        print(new.formula)
        print(fixed.other.values.frame)
        print(term.labels)
        print(focus.variable.model.matrix)
        print(expanded.term.labels)
        print(focus.term.occurrence)
        print(power.vec)
    }
    xi.mat <- matrix(0, ncol = length(conditional.names), nrow = length(groupm.out$theta.hat) -
        1)
    for (i in 1:ncol(xi.mat) - 1) {
        if (class.focus)
            the.ones <- focus.term.occurrence[i, ]
        else the.ones <- power.vec == unique.power[i]
        xi.mat[the.ones, i] <- focus.variable.model.matrix[the.ones]
    }
    xi.mat <- rbind(xi.mat, c(rep(0, length = length(conditional.names) -
        1), 1))
    if (map.SMRDDebugLevel() > 3)
        print(xi.mat)
    gvec <- as.vector(groupm.out$theta.hat %*% xi.mat)
    gmat <- t(xi.mat) %*% groupm.out$vcv %*% xi.mat
    names(gvec) <- conditional.names
    dimnames(gmat) <- list(conditional.names, conditional.names)
    conditional.groupm.out$terms <- new.terms
    orig.group.var <- conditional.groupm.out$group.var
    conditional.groupm.out$group.var <- conditional.groupm.out$group.var[focus.variable.index]
    conditional.groupm.out$focus.variable <- focus.variable
    conditional.groupm.out$theta.hat <- gvec
    conditional.groupm.out$vcv.matrix <- gmat
    conditional.groupm.out$relationship <- subscript.relationship(groupm.out$relationship,
        focus.variable.index)
    the.strings <- ClistToVec(fixed.other.values, sep = ";")
    fixed.var.names <- variable.names[-focus.variable.index]
    title.string <- paste("Fixed values of", paste(paste(fixed.var.names,
        the.strings, sep = "="), collapse = ", "))
    conditional.groupm.out$title <- paste(get.data.title(groupm.out$data.ld),
        " ", paste(get.xlabel(groupm.out$data.ld)[groupm.out$group.var],
            name.relationship(groupm.out$relationship, allow = T),
            sep = "", collapse = ", "), paste(", Dist:", groupm.out$distribution,
            sep = ""), "\n", title.string, sep = "")
    if (map.SMRDDebugLevel() > 4)
        print(conditional.groupm.out)
    invisible(conditional.groupm.out)
}

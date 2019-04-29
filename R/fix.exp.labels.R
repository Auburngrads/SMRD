fix.exp.labels <-
function (ticlabels) 
{
    ticlabels.orig <- ticlabels
    nchar.out <- nchar(ticlabels)
    where.zero <- rep(0, length(nchar.out))
    plus.or.neg <- rep("0", length(nchar.out))
    mantissa <- rep(NA, length(nchar.out))
    e.here <- rep(NA, length(nchar.out))
    power <- rep(NA, length(nchar.out))
    charmat <- matrix(NA, nrow = length(nchar.out), ncol = max(nchar.out))
    ticlabels <- paste(ticlabels, "     ")
    for (i in 1:max(nchar.out)) {
        charmat[, i] <- substring(ticlabels, i, i)
        e.here[charmat[, i] == "e"] <- i
    }
    strip.lead0 <- function(xstring) {
        nchar.out <- nchar(xstring)
        fchar <- substring(xstring, 1, 1)
        xstring <- ifelse(fchar == "0", substring(xstring, 2, 
            nchar.out), xstring)
        return(xstring)
    }
    #
    # parse each label where scientific notation was used
    #
    worknow <- !is.na(e.here)
    if (any(worknow)) {
        plus.or.neg[worknow] <- substring(ticlabels[worknow], 
            e.here[worknow] + 1, e.here[worknow] + 1)
        mantissa[worknow] <- substring(ticlabels[worknow], 1, 
            e.here[worknow] - 1)
        power[worknow] <- strip.lead0(substring(ticlabels[worknow], 
            e.here[worknow] + 2, nchar.out[worknow]))
        was.exp <- !is.na(mantissa)
    #
    # we will suppress any mantissas that have value 1
    #
        man1 <- mantissa == "1"
    }
    was.exp <- !is.na(mantissa)
    plus.or.neg[plus.or.neg != "-"] <- ""
    power <- paste(plus.or.neg,power,sep="")
    #
    #getting expressions for scientific notation on plot axis labels
    #
     for(i in 1:length(was.exp)){
      if(was.exp[i]){
    if(man1[i]){
      ticlabels.orig[i] <- as.expression(bquote(10^ .(power[i])))
    }
    else{
      ticlabels.orig[i] <- as.expression(bquote(.(mantissa[i]) %*%10^  .(power[i])))
    }
  }
    }
    return(ticlabels.orig)
}

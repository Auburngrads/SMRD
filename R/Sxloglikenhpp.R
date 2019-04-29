Sxloglikenhpp <-
function (time, RecurrCosts, timel, timeu, kwcount, form, theta) 
{
    answerf <- sum(RecurrCosts * flogrecurrate(time, form, theta))
    answerw <- sum(kwcount * fmcfdiff(timel, timeu, form, theta))
    answer <- answerf - answerw
    fmcf(timeu, form, theta)
    return(answer)
}

good.data <-
function (data.ld, 
          check.level = SMRDOptions("SMRD.DataCheck"), 
          number.needed = 2) 
{
    switch(check.level, strong = {
        number.failures.needed <- 2
        number.cells.needed <- 3
    }, moderate = {
        number.failures.needed <- 1
        number.cells.needed <- 2
    }, weak = {
        number.failures.needed <- 0
        number.cells.needed <- 1
    }, none = {
        number.failures.needed <- -5
        number.cells.needed <- -5
    }, {
        stop(paste(check.level, "is an unrecognized check level"))
    })
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    number.code1 <- the.case.weights[the.censor.codes == 1 | 
        the.censor.codes == 5]
    number.code2 <- the.case.weights[the.censor.codes == 2]
    number.code4 <- the.case.weights[the.censor.codes == 4]
    number.code3 <- the.case.weights[the.censor.codes == 3]
    if (length(number.code1) > 0 && length(number.code3) + length(number.code4) == 0) {
      
        check.failures <- number.failures.needed + number.needed - 1
        
        `if`(length(number.code2) == 0,
             conclusion <- length(number.code1) >= check.failures,
             conclusion <- length(number.code1) >= check.failures - 1)
        
  } else {
    
        check.failures <- number.failures.needed + number.needed - 1
        number.cells <- length(number.code1) + length(number.code2) + length(number.code3) + length(number.code4)
        
        `if`(length(number.code3) > 0,
             count.left <- 1,
             count.left <- 0)
        
        number.failures <- sum(number.code1) + count.left + sum(number.code4)
        conclusion <- number.cells > number.cells.needed && number.failures > check.failures
        
        attr(conclusion, "check.level") <- check.level
        return(conclusion)
        
  }
    
}
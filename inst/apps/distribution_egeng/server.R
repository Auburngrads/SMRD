server = function(input, output, session) {
  
     SMRD::add_server('distribution_egeng_functions', env = environment())

output$egengfunc  <- renderUI({ SMRD::add_rmd('egeng-func.Rmd') })

output$egengprops <- renderUI({ SMRD::add_rmd('egeng-props.Rmd') })
}
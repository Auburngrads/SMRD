server = function(input, output, session) {
  
     SMRD::add_server('distribution_geng_functions', env = environment())

output$gengfunc  <- renderUI({ SMRD::add_rmd('geng-func.Rmd') })

output$gengprops <- renderUI({ SMRD::add_rmd('geng-props.Rmd') })
}
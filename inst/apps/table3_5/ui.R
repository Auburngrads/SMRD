ui = navbarPage(title = 'Table 3.5',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

                
    tabPanel('Table 3.5', DT::dataTableOutput('table5')))


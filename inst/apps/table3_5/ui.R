ui = navbarPage(title = 'Table 3.5',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),

                
    tabPanel('Table 3.5', DT::dataTableOutput('table5')))


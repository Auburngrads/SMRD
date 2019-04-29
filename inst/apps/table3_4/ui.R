ui = navbarPage(title = 'Table 3.4',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),

        
      tabPanel('Data Set',DT::dataTableOutput("table.shock1", height = "80%")),
      tabPanel('Data Set',DT::dataTableOutput("table.shock2", height = "80%")))

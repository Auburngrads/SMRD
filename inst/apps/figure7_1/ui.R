ui = navbarPage(title = 'Figure 7.1',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

                
tabPanel('Figure 7.1',
   sidebarLayout(
     sidebarPanel(width = 3,
       selectInput("n7.1", 
                   label = "Observations:",
                   choices = c("20", "200", "2000", "10220"), 
                   selected = "20")),
    
     mainPanel(plotOutput('berkson'), width = 9))),
    
tabPanel('Table 7.1', DT::dataTableOutput('table.1', height = '80%')),
tabPanel('Code Mirror', codemirrorR::codemirrorOutput('cm.table.1', 
                                                          height = '600px')))

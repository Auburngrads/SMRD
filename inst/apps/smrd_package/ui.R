ui = navbarPage(title = 'R Package: SMRD',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

                
tabPanel('Background',
         mainPanel(uiOutput('smrd.back'), class = 'shiny-text-output', width = 12)),

tabPanel('Features',
         mainPanel(uiOutput('smrd.feat'), class = 'shiny-text-output', width = 12)),
                
tabPanel("Data Sets", DT::dataTableOutput("smrd.data")),

tabPanel('Vignettes',
         mainPanel(uiOutput('smrd.vign'), class = 'shiny-text-output', width = 12)))

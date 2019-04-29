ui = navbarPage(title = 'Example 1.2',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),


tabPanel('Background',
         mainPanel(uiOutput('example1.2'), class = 'shiny-text-output', width = 12)),
                
tabPanel("Data Set",
         titlePanel(HTML('SMRD data object: <code>lfp1370</code>')),
         DT::dataTableOutput("lfp1370", height = "575px")),
                
tabPanel('Figure 1.3',
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor(fontSize = 16, 
                          wordWrap = T,
                          outputId = "fig3plot", 
                          mode = "r", 
                          theme = "github", 
                          height = "450px", 
                          
                          value = 
"IC.ld<-frame.to.ld(frame = SMRD::lfp1370,\nresponse.column = 1,\ncensor.column = 2,\ncase.weight.column = 3,\ndata.title = 'IC Failure Data',\ntime.units = 'Hours')\n\npar(family='serif',font=2,bg=NA)\n\nevent.plot(IC.ld)\n\n\n\n# event.plot( ) is part of the SMRD package\n\n# SMRD commands can only be performed on\n# 'life.data' objects\n\n# frame.to.ld( ) converts a 'data.frame'\n# to a 'life.data' object"),

        actionButton("evalfig3", 'Evaluate', width = '100%')),
        
        mainPanel(plotOutput("plotfig3"), width = 7))))

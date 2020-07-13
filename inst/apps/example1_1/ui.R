ui = navbarPage(title = 'Example 1.1',
                position = 'fixed-top',
                collapsible = T, 
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),
                
tabPanel('Background',
         mainPanel(uiOutput('example1.1'), class = 'shiny-text-output', width = 12)),
                
tabPanel("Table 1.1", 
         titlePanel(HTML('SMRD data object: <code>lzbearing</code>')),
         DT::dataTableOutput("lzbearing")),

navbarMenu('Figures', icon = icon('bar-chart-o'),       
tabPanel("Figure 1.1",
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor(fontSize = 16, 
                          wordWrap = T,
                          outputId = "fig1plot", 
                          mode = "r", 
                          theme = "github", 
                          height = "450px", 
                          value = "
library(SMRD)

par(family = 'serif', font = 2)

hist(SMRD::lzbearing$megacycles,
     breaks = seq(0,200,20),
     col = 'black',
     border = 'white',
     prob = TRUE,
     main = 'Figure 1.1 - Histogram of the ball bearing failure data',
     las = 1)"),
      
        actionButton("evalfig1", "Evaluate")),
        
        mainPanel(plotOutput("plotfig1"), width = 7))),

tabPanel("Figure 1.2",
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor(fontSize = 16, 
                          wordWrap = T,
                          outputId = "fig2plot", 
                          mode = "r", 
                          theme = "github", 
                          height = "450px", 
                          value = 
"library(SMRD)

par(family='serif', font=2)

lzbearing.ld <- frame.to.ld(SMRD::lzbearing, 
                            response.column = 1, 
                            time.units = 'Megacycles')

event.plot(lzbearing.ld)"),

        actionButton("evalfig2", "Evaluate")),
        
        mainPanel(plotOutput("plotfig2"), width = 7)))))

ui = navbarPage(title = 'Figure 1.1',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),


tabPanel("Data Set", DT::dataTableOutput("lzbearing")),
                
tabPanel("Figure 1.1",titlePanel("Edit this code and press 'Evaluate' to change the figure"),
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor("fig1plot", mode = "r", theme = "github", height = "450px", fontSize = 15,
                      value = "library(SMRD)
par(family='serif', font=2)
hist(lzbearing$megacycles,
     breaks=seq(0,200,20),
     col='black',
     border='white',
     prob=TRUE,
     main='Figure 1.1 - Histogram of the ball bearing failure data',
     las = 1)"),
              actionButton("evalfig1", "Evaluate")),
        
        mainPanel(plotOutput("plotfig1"), width = 7))),

tabPanel("Figure 1.2",titlePanel("Edit this code and press 'Evaluate' to change the figure"),
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor("fig2plot", 
                          mode = "r", 
                          theme = "github",
                          height = "450px", 
                          fontSize = 15,
                          value = "
library(SMRD)
par(family='serif', font=2)

lzbearing.ld <- frame.to.ld(lzbearing, 
                            response.column = 1, 
                            time.units = 'Megacycles')

event.plot(lzbearing.ld)"),
      
        actionButton("evalfig2", "Evaluate")),
        
        mainPanel(plotOutput("plotfig2"), width = 7))))


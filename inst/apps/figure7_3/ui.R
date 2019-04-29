ui = fluidPage(theme = add_theme(getShinyOption('theme')),
               add_css(),
             
sidebarLayout(
  sidebarPanel(width = 3,
    selectInput("n7.3", 
                label = "Observations:",
                choices = c("20", "200", "2000", "10220"), 
                selected = "20")),
     
     mainPanel(plotOutput('berksonmle'), width = 9)))

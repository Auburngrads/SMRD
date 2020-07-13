ui = navbarPage(title = 'Example 7.1',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

                
tabPanel('Background',
         mainPanel(uiOutput('example7.1'), class = 'shiny-text-output', width = 12)),
                
tabPanel("Data", DT::dataTableOutput("berkson")),

navbarMenu('Figures', icon = icon('bar-chart-o'),
tabPanel("Figure 7.1",
fluidRow(column(width = 5,
    sidebarPanel(width = '100%',
      shinyAce::aceEditor(fontSize = 16,
                          wordWrap = T,
                          outputId = "fig71plot",
                          mode = "r",
                          theme = "github",
                          height = "450px",
                          value = "
sample.size <- 20

berkson <- 
switch(as.character(sample.size), 
      '20' = {SMRD::berkson20},
     '200' = {SMRD::berkson200},
    '2000' = {SMRD::berkson2000},
   '10220' = {SMRD::berkson10220})

berkson.ld <- 
frame.to.ld(berkson,
            response.column = c(1,2),
            censor.column = 3,
            case.weight.column = 4)

plot(berkson.ld, distribution = 'Exponential')"),

        actionButton("evalfig71", "Evaluate"))),

column(width = 7, 
       mainPanel(width = '100%',plotOutput("plotfig71")))))))

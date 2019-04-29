ui = navbarPage(title = 'Heat Exchanger Example',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),

                
tabPanel("Data Set",   DT::dataTableOutput("table.heat", height = "80%") ),

tabPanel("Summary", verbatimTextOutput("summary.heat") ), 

tabPanel("Event Plots",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("PLOT_4", label = "Plot:",
                      choices = c("Event Plot",
                                  "Histogram"),
                      selected = "Event Plot")),  

mainPanel( plotOutput("eventplot.heat", height = '650px'), width = 9))),

tabPanel("CDF Plot",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("DIST_4", 
            label = "Distribution:",
            choices = c("Weibull",
                        "Exponential",
                        "Normal",
                        "Lognormal",
                        "Smallest Extreme Value",
                        "Largest Extreme Value",
                        "Frechet"), 
            selected = "Weibull"),

selectInput("CI_4",
            label = "Confidence Level:",
            choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
            selected = 0.95),
                   
selectInput("BT_4",
            label = "Band Type:",
            choices = c("Pointwise", "Simultaneous", "none"), 
            selected = "Pointwise")),  

mainPanel( plotOutput("cdfplot.heat", height = '650px'), width = 9))))

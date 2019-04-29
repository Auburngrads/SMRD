ui = navbarPage(title = 'at7987',
                collapsible = T, 
                position = 'fixed-top',
                theme  = teachingApps::add_theme(getShinyOption('theme')),
                header = teachingApps::add_css(),
                footer = teachingApps::add_logo(),


tabPanel("Data Set",DT::dataTableOutput("table.at7987", height = "80%") ), 

tabPanel("Summary", verbatimTextOutput("summary.at7987") ), 

tabPanel("Event Plots",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("PLOT_2", label = "Plot:",
            choices = c("Event Plot","Histogram"),
            selected = "Event Plot")),  
mainPanel( plotOutput("eventplot.at7987", height = '650px'), width = 9))),

tabPanel("CDF Plot",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("dist_2", 
            label = "Distribution:",
            choices = c("None",
                        "Weibull",
                        "Exponential",
                        "Normal",
                        "Lognormal",
                        "Smallest Extreme Value",
                        "Largest Extreme Value","Frechet"), 
            selected = "Weibull"),

selectInput("ci_2", label = "Confidence Level:",
                    choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
                    selected = 0.95),
                   
selectInput("bt_2",   
            label = "Band Type:",
            choices = c("Pointwise", 
                        "Simultaneous", 
                        "none"), 
            selected = "Pointwise")),  

mainPanel( plotOutput("cdfplot.at7987", height = '650px'), width = 9))),

tabPanel('Code Mirror', 
         mainPanel(codemirrorR::codemirrorOutput('figures', height = '650px'), width = 12)))


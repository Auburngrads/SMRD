ui = navbarPage(title = 'Bleed Data Set',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

                
tabPanel("Data Set",   DT::dataTableOutput("table.bleed", height = "80%") ), 

tabPanel("Summary", 
mainPanel(width = 12,
  tabsetPanel(
    tabPanel("Bases: All",   verbatimTextOutput("summary.bleed")),
    tabPanel("Bases: D",     verbatimTextOutput("summary.bleed.d")),
    tabPanel("Bases: Other", verbatimTextOutput("summary.bleed.o"))))), 

tabPanel("Event Plots",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("PLOT_6", label = "Plot:",
                    choices = c("Event Plot","Histogram"),
                    selected = "Event Plot")),  
mainPanel( width = 9,
  tabsetPanel(
tabPanel("Bases: All",   plotOutput("eventplot.bleed")),
tabPanel("Bases: D",     plotOutput("eventplot.bleed.d")),
tabPanel("Bases: Other", plotOutput("eventplot.bleed.o"))
)))),

tabPanel("CDF Plot",
sidebarLayout(
sidebarPanel(width = 3,
selectInput("DIST_6", label = "Distribution:",
                    choices = c("Weibull",
                                "Exponential",
                                "Normal",
                                "Lognormal",
                                "Smallest Extreme Value",
                                "Largest Extreme Value","Frechet"), 
                    selected = "Weibull"),

selectInput("CI_6", label = "Confidence Level:",
                    choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
                    selected = 0.95),
                   
selectInput("BT_6", label = "Band Type:",
                    choices = c("Pointwise", "Simultaneous", "none"), 
                    selected = "Pointwise")),  

mainPanel( width = 9,
  tabsetPanel(
tabPanel("Bases: All",   plotOutput("cdfplot.bleed")),
tabPanel("Bases: D",     plotOutput("cdfplot.bleed.d")),
tabPanel("Bases: Other", plotOutput("cdfplot.bleed.o"))
)))),

tabPanel('Code Mirror', 

mainPanel(codemirrorR::codemirrorOutput('figures.bleed'), 
          width = 12)))

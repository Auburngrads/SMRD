bent_probplot <- 

function(theme = "flatly", storyteller = F, width = '100%',
         height = `if`(storyteller,'800px','600px'),
         more.opts = list(NA),...) {

    dir <- dirname(system.file("apps", "bent_probplot", "global.R", package = "SMRD"))
    
    teachingApps::getPackage(pkg = 'harrypotter', repo = 'bradleyboehmke')
    
    assign.shiny.opts(opts = more.opts,
                      dir = dir,
                      theme = theme,
                      story = storyteller)
    
    shiny::shinyAppDir(appDir = dir, options = list(height = height, width = width,...))

}

#' Render an SMRD App With Options
#'
#' @description Renders an SMRD App as a stand-alone shiny app
#'     or as an element within an rmarkdown document. 
#'
#' @param app_name \code{character} Name of the app to be rendered
#' @param theme \code{character} Name of a bootswatch color theme (provided by \code{shinythemes::shinytheme})
#' @param width \code{character} The width of the printed app (in pixels)
#' @param height \code{character} The height of the printed app (in pixels)
#' @param icon  \code{character} A fontAwesome icon to be placed in the footer of a navbarPage app
#' @param img \code{character} A path (or URL) to an image to be placed in the footer of a navbarPage app
#' @param git_user \code{character} GitHub username used in the branding logo
#' @param more_opts A \code{list} of additional options/objects that can be passed to the app (see Details)
#' @param launch.browser \code{logical} If \code{TRUE} The app launches in the user's default browser
#' @param ... A \code{list} of additional options passed to \code{shiny::shinyAppDir()} 
#' 
#' @importFrom utils View
#' @importFrom shiny shinyAppDir
#' @return A printed shiny app
#' @export
#'
#' @seealso \code{link{create_logo}}
#' @seealso \code{link{add_logo}}
#' @examples 
#' \dontrun{
#' smrd_app(app_name = 'at7987_data', 
#'          theme = 'spacelab', 
#'          height = '800px')
#' 
#' smrd_app(app_name = 'berkson_interval',
#'          theme = 'flatly',
#'          height = '600px')
#' }
smrd_app <- 
function(app_name = NULL, 
         theme = 'flatly',
         width = '100%',
         height = '800px',
         icon = 'fa fa-github',
         img = NULL,
         git_user = 'Auburngrads',
         more_opts = list(NA),
         launch.browser = TRUE,...)
{
 
    valid_apps <- list.files(system.file("apps", package = "SMRD"))

    valid_apps_df <- data.frame(valid_apps)
    colnames(valid_apps_df) <- 'Valid smrd_apps'
    
    if (missing(app_name) || !nzchar(app_name) || !app_name %in% valid_apps) {
      
    stop(paste0('Please run `smrd_app()` with a valid app as an argument.\n',
                 "See table for Valid SMRD Apps"),
         utils::View(valid_apps_df),
         call. = FALSE)
    }
    
   
    dir <- system.file('apps', app_name, package = 'SMRD')
  
      add_options(opts = more_opts,
                              dir = dir,
                              theme = theme,
                              icon = icon,
                              img = img,
                              git_user = git_user)
    
    shiny::shinyAppDir(appDir = dir, 
                       options = list(height = height, 
                                      width = width,
                                      launch.browser = launch.browser,...))

}
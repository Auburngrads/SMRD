#' @importFrom plot3D persp3D
#' @importFrom plotly plot_ly add_surface layout
wqm.persp <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1, length.out = ncol(z)), 
            z,
            xlim = range(x),
            ylim = range(y),
            zlim = range(z, na.rm = TRUE),
            xlab = "X",
            ylab = "Y",
            zlab = "Z",
            main = NULL,
            sub = NULL,
            theta = 40,
            phi = 20,
            r = sqrt(3),
            d = 1,
            scale = TRUE,
            expand = 1,
            border = NULL,
            col = NULL, 
            ltheta = -135,
            lphi = 0,
            shade = NA,
            box = TRUE,
            axes = TRUE,
            nticks = 5,
            ticktype = "simple",
            colkey = NULL,
            static = static,...)
  {
    if(static){
      
    plot3D::persp3D(x = x, y = y, z = z, xlim = xlim, ylim = ylim,
                    zlim = zlim, xlab = parse(text = xlab), ylab = parse(text = ylab), zlab = zlab,
                    main = main, sub = sub, theta = theta, phi = phi,
                    r = r, d = d, scale = scale, expand = expand,
                    border = border, ltheta = ltheta, lphi = lphi, shade = shade,
                    box = box, axes = axes, nticks = nticks, ticktype = ticktype,
                    col = plot3D::gg.col(100), colkey = colkey,bty = "g",...)
      
    } else {
      
      p = plotly::plot_ly(z = z,
                          x = x, 
                          y = y, 
                          width = 800, 
                          height = 800)
   
   contours = list(z = list(show = TRUE,
                            usecolormap = TRUE,
                            highlightcolor = "#ff0000",
                            project = list(z = TRUE)))
   
   p <- plotly::add_surface(p, contours = contours)
   
   axs_titlefont <- list(family = "Arial, sans-serif",
                         size = 18,
                         color = "black")
   
   xaxs <- list(title = parse(text = xlab),
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   yaxs <- list(title = parse(text = ylab),
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   zaxs <- list(title = zlab,
                titlefont = axs_titlefont,
                showticklabels = TRUE)
   
   p <- plotly::layout(p, scene = list(xaxis = xaxs, yaxis = yaxs, zaxis = zaxs))

   print(p)
      
    }
  }
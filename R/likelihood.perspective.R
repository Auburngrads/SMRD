likelihood.perspective <-
  function (struct, profile.title = paste(struct$subtitle, "\n",
                                          lplot.type, "Likelihood for", variable.namex, "and", variable.namey,
                                          "\n", model.dist.str), variable.namex = struct$xlab, variable.namey = struct$ylab,
            transformationx = "linear", transformationy = "linear", original.par = F,
            pretty.x = NULL, pretty.y = NULL, theta = 40, phi = 20, r = sqrt(3))
  {
    do.mixed.text <- is.postsctiptok() && substring(variable.namex,
                                                    1, 1) == "~"
    if (!is.null(struct$number.parameters) && struct$number.parameters ==
        2)
      lplot.type <- "Relative"
    else lplot.type <- "Profile"
    model.dist.str <- NULL
    if (is.null(struct$distribution)) {
      if (is.null(struct$form)) {
        model.dist.str <- NULL
      } else {
        model.dist.str <- paste("from the", struct$form,
                                " Model")
      }
    } else {
      if (is.null(struct$form)) {
        model.dist.str <- paste("from the", struct$distribution,
                                "Distribution")
      } else {
        model.dist.str <- paste("from the", struct$distribution,
                                struct$form, " Model")
      }
    }
    if (profile.title == "")
      top.mar <- 4
    else top.mar <- 7
    if (original.par) {
      old.par <- par(mar = c(4.5, 5, 3.5, 2) + 0.1, cex = 1.1)
      on.exit(par(old.par))
    }
    cex.lab<- 1.1
    if (do.mixed.text) {
      wqm.persp(x = struct$x, y = struct$y, z = struct$z, xaxt = "n",
                yaxt = "n", xlab = "", ylab = "", theta = theta,
                phi = phi, r = r, zlab = "Relative Likelihood")
      mixed.text(x.loc(0.7), y.loc(0.1), texts = variable.namex,
                 cex = 1.5 * cex.lab)
      mixed.text(x.loc(0.02), y.loc(0.2), texts = variable.namey,
                 cex = 1.5 * cex.lab)
    } else {
      wqm.persp(x = struct$x, y = struct$y, z = struct$z, xlab = variable.namex,
                ylab = variable.namey, zlab = "Relative Likelihood",
                theta = theta, phi = phi, r = r)
    }
    return(invisible())
  }

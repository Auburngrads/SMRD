wqm.persp <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1,
                                                         length.out = ncol(z)), z, xlim = range(x), ylim = range(y),
            zlim = range(z, na.rm = TRUE), xlab = "X", ylab = "Y", zlab = "Z",
            main = NULL, sub = NULL, theta = 40, phi = 20, r = sqrt(3),
            d = 1, scale = TRUE, expand = 1, border = NULL, col = NULL, 
            ltheta = -135, lphi = 0, shade = NA, box = TRUE, axes = TRUE,
            nticks = 5, ticktype = "simple", colkey = NULL,...)
  {
    persp3D(x = x, y = y, z = z, xlim = xlim, ylim = ylim,
            zlim = zlim, xlab = parse(text = xlab), ylab = parse(text = ylab), zlab = zlab,
            main = main, sub = sub, theta = theta, phi = phi,
            r = r, d = d, scale = scale, expand = expand,
            border = border, ltheta = ltheta, lphi = lphi, shade = shade,
            box = box, axes = axes, nticks = nticks, ticktype = ticktype,
            col = col, colkey = colkey,bty = "b2",...)
  }
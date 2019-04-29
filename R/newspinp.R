newspinp <-
function (struct, levels = c(0.001, 0.01, 0.2, 0.4, 0.6, 0.8, 
    0.9)) 
{
    struct$z[struct$z <= 1e-10] <- 1e-10
    repeat {
        contour(struct, levels = levels, xlab = "", ylab = "")
        title(main = "Profile Contour Grid", xlab = struct$xlab, 
            ylab = struct$ylab)
        cat("Input number of steps, distance, and height: ")
        vec <- f.getvec()
        if (is.na(vec[1])) 
            break
        theta <- seq(0, 2 * pi, length = vec[1] + 1)
        eye <- cbind(cos(theta) * vec[2], sin(theta) * vec[2], 
            rep(vec[3], length(theta)))
        print(eye)
        saveeye <- c(8, 8, 8)
        for (i in 1:length(theta)) {
            persp(x = struct$x, y = struct$y, z = struct$z, theta = theta[i], 
                phi = 15, r = sqrt(3), xlab = struct$xlab, ylab = struct$ylab, 
                zlab = "Profile Likelihood")
            title(main = "Profile Likelihood Grid")
            newpause.ans <- newpause()
            if (newpause.ans == "n") 
                break
            if (newpause.ans == "s") 
                save.theta <- theta[i]
        }
    }
    cat("\nThe eye is", paste(saveeye, collapse = ","), "\n")
    double.profile(struct, theta = save.theta, phi = 15, r = sqrt(3))
    invisible()
}

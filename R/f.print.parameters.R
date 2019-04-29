f.print.parameters <-
function (mlest.out, print.parameters, param.loc = "bottomright")
{
    old.options <- options(digits = 4)
    on.exit(options(old.options))
    if (print.parameters) {

            switch(generic.distribution(mlest.out$distribution),
                weibull = {
                  string1 <- parse(text = paste("widehat(eta)==", format(exp(mlest.out$theta.hat[1]))))
                  string2 <- parse(text = paste("widehat(beta)==", format(1/mlest.out$theta.hat[2])))
                }, exponential = {
                  string1 <- ""
                  string2 <- parse(text = paste("widehat(theta)==", format(exp(mlest.out$theta.hat[1]))))
                }, {
                  string1 <- parse(text = paste("widehat(mu)==", format(mlest.out$theta.hat[1])))
                  string2 <- parse(text = paste("widehat(sigma)==", format(mlest.out$theta.hat[2])))
                })

      xshift<-0
      yshift<-0

      if (param.loc=="topright"   ) yshift<-0.7
      if (param.loc=="bottomleft" ) xshift<-0.7
      if (param.loc=="topleft"    ) {

        yshift<-0.7 ; xshift<-0.7
      }

            text(x.loc(0.75 - xshift), y.loc(0.18 + yshift), string1, adj = 0, cex = 1.25)
            text(x.loc(0.75 - xshift), y.loc(0.07 + yshift), string2, adj = 0, cex = 1.25)
    }
    invisible()
}

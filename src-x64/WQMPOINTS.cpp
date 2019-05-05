#include <base/base.hpp>
#include <wqmpoints/wqm_points.hpp>

//' Wrapper for wqm_points
// [[Rcpp::export]]
Rcpp::List WQMPOINTS(Rcpp::NumericVector &q,
                     Rcpp::NumericVector &p,
                     Rcpp::NumericVector &prob,
                     Rcpp::NumericVector &sd,
                     int &lsd,
                     int &m,
                     Rcpp::NumericVector  &yplot,
                     Rcpp::NumericVector  &pplot,
                     Rcpp::NumericVector &sdplot,
                     int &mplot){
  
   wqm_points(q,p,prob,sd,lsd,m,yplot,pplot,sdplot,mplot);
  
   return Rcpp::List::create(Named("q") = q,
                             Named("p") = p,
                             Named("prob") = prob,
                             Named("sd") = sd,
                             Named("lsd") = lsd,
                             Named("m") = m,
                             Named("yplot") = yplot,
                             Named("pplot") = pplot,
                             Named("sdplot") = sdplot,
                             Named("mplot") = mplot);
  
}
      
#include <base/base.hpp>

void wqm_points(Rcpp::NumericVector &q,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd,
                int &lsd,
                int &m,
                Rcpp::NumericVector  &yplot,
                Rcpp::NumericVector  &pplot,
                Rcpp::NumericVector &sdplot,
                int &mplot) {

  double pnow   ;
  double sdnow  ;
  double delta  ;
  double denom  ;

   for(int j = 0; j < m; j++) {

     sdnow = 0.0;

     if(prob.at(j) <= 0.0e00) { continue; }

        pnow = prob.at(j) ;

     if(lsd == 1) { sdnow = sd.at(j); }

     if(j == 0)   {

       if((pnow <= 0.0e00) or (pnow >= 1.0e00)) continue;

       yplot.at(mplot) = p.at(j) ;
       pplot.at(mplot) = pnow    ;
       sdplot.at(mplot) = sdnow  ;
       mplot = mplot + 1         ;

     } else {

       delta = 1.0e00 ;
       denom = (p.at(j) - p.at(j - 1)) ;

    if(denom <= 0.0e00) {

      pnow = delta * prob.at(j - 1) + (1.0e00 - delta) * prob.at(j) ;

      sdnow = delta * sd.at(j - 1) + (1.0e00 - delta) * sd.at(j) ;

      if((pnow <= 0.0e00) or (pnow >= 1.0e00)) continue;

      yplot.at(mplot) = p.at(j) ;
      pplot.at(mplot) = pnow    ;
      sdplot.at(mplot) = sdnow  ;
      mplot = mplot + 1         ;

    } else {

       delta = ((q.at(j - 1) - p.at(j - 1)) / (denom)) / 2.0e00 ;

       pnow = delta * prob.at(j - 1) + (1.0e00 - delta) * prob.at(j) ;

       sdnow = delta * sd.at(j - 1) + (1.0e00 - delta) * sd.at(j) ;

    if((pnow <= 0.0e00) or (pnow >= 1.0e00)) continue;

       yplot.at(mplot) = p.at(j) ;
       pplot.at(mplot) = pnow    ;
       sdplot.at(mplot) = sdnow  ;
       mplot = mplot + 1         ;

     }
     }
   }

return;
   
}

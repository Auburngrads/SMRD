#include <base/base.hpp>
#include <utility/dcheck.hpp>

//' This needs work to return error an message 
//' if a problem has been found

void vtran(Rcpp::NumericMatrix &mat,
           int n_col,
           int n_row,
           int itran,
           int &ier){
  
ier = 0;
if(itran == 1) return;

for(int i = 1; i <= n_row; i++){
  
    for(int j = 1; j <= n_col; j++){
      
        if(itran == 2) {
          
           dcheck(mat.at(i - 1,j - 1),1.0e-20,1.0e30,1.0e-20,1.0e30,ier,i - j);
           mat.at(i - 1,j - 1) = std::log(mat.at(i - 1,j - 1));
          
        }
        if(itran == 3) {
          
           dcheck(mat.at(i - 1,j - 1),-273.0,500.0,-272.9,500.0,ier,i - j);
           mat.at(i - 1,j - 1) = 1.0 / (mat.at(i - 1,j - 1) + 273.0);
          
        }
        if(itran == 4) {
          
           dcheck(mat.at(i - 1,j - 1),-160.0,160.0,-160.0,160.0,ier,i - j);
           mat.at(i - 1,j - 1) = std::exp(mat.at(i - 1,j - 1));
          
        }
    }
}
 
      return;
  
}

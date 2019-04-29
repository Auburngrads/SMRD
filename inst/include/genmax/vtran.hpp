#ifndef smrd_vtran_H
#define smrd_vtran_H
void vtran(Rcpp::NumericMatrix &mat,
           int n_col,
           int n_row,
           int itran,
           int &ier);
#endif
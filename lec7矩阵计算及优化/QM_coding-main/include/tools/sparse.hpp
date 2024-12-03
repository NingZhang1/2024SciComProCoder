#ifndef SPARSE_HPP
#define SPARSE_HPP

#include <cstddef>
#include <iostream>
#include <Eigen/Dense>

void CSR_Dim(double **Mat, int n, int m, int *n0, double thre);
void Mat_to_CSR(double **Mat, int n, int m, double *Va, int *Ja, int *Ia, double thre, int io);
void CSR_to_Mat(double *Va, int *Ja, int *Ia, double **Mat, int n, int m, int io);
void amub(int nrow, int ncol, double *a, int *ja, int *ia, double *b, int *jb, int *ib, double *c, int *jc, int *ic);	

#endif

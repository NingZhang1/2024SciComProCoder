#ifndef LINEAR_HPP
#define LINEAR_HPP

#include <cstddef>
#include <iostream>
#include <Eigen/Dense>

using namespace Eigen;

/* -----------------------------------------------------------------------------
   Definition of linear algebra tools
   ----------------------------------------------------------------------------- */

/* ----------------------------------------- */
/* definition of vectors                     */
/* ----------------------------------------- */
double    *make_dvector(int nl,int nh);

/* ------------------------------------------- */
/* defintion of matrices                       */
/* ------------------------------------------- */
double    **make_dmatrix(int nrl,int nrh,int ncl,int nch);

/* ------------------------------------------- */
/* free the memory for vectors                 */
/* ------------------------------------------- */
void free_dvector(double *v   ,int nl,int nh);

/* ------------------------------------------- */
/* free the memory for matrices                */
/* ------------------------------------------- */
void free_dmatrix(double **m   ,int nrl,int nrh,int ncl,int nch);

/* ------------------------------------------- */
/* print matrices                              */
/* ------------------------------------------- */
void print_dmatrix(double **m,int nrl,int nrh,int ncl,int nch);

/* ------------------------------------------- */
/* print vectors                               */
/* ------------------------------------------- */
void print_dvector(double *v,int nl,int nh);

double *make_dvector(int a);
void free_dvector(double *m,int a);

double **make_dmatrix(int a,int b);
void free_dmatrix(double **m,int a, int b);

int Retrieve_SymMatrix( FILE *f, MatrixXd &matrix);
int Store_SymMatrix( FILE *f, MatrixXd &matrix);

int Retrieve_Matrix( FILE *f, MatrixXd &matrix);
int Store_Matrix( FILE *f, MatrixXd &matrix);

char *Up(char *Result, char *s);
char *Lo(char *Result, char *s_);

#endif

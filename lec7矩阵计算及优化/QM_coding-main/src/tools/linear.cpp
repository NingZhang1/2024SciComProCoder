#include <tools/linear.hpp>
#include <iostream>

// Old constructor for matrix and vector
double *make_dvector(int a) {
    return(make_dvector(0, a-1));
}
/*---------------------------------------------------------*/
/* Make a vector                                           */
/*---------------------------------------------------------*/
double *make_dvector(int nl, int nh)
{  double *v;
   v= new double[nh-nl+1];
   if (!v) { return NULL; };
   return v-nl;
}

void free_dvector(double *m,int a) {
    free_dvector(m,0,a-1);
}
/*---------------------------------------------------------*/
/* Delete a vector                                         */
/*---------------------------------------------------------*/
void free_dvector(double *v,int nl,int nh)
{ v+=nl;
  delete[] v;
  //v = NULL;
}

double **make_dmatrix(int a,int b) {
    return(make_dmatrix(0,a-1,0,b-1));
}

/*---------------------------------------------------------*/
/* Make a matrix                                           */
/*---------------------------------------------------------*/
double **make_dmatrix(int nrl,int nrh,int ncl,int nch)
{ int i,j;
  double **m;
  /* --------------------------------- */
  /* allocate an array of pointers     */
  /* --------------------------------- */
  m= new double *[nrh-nrl+1];
  if (!m) { return 0; };
  m -= nrl;
  /* --------------------------------------- */
  /* allocate a vector of numbers for        */
  /* every pointer                           */
  /* --------------------------------------- */
  for(i=nrl;i<=nrh;i++){
    m[i]= new double[nch-ncl+1];
    /* ---------------------------------------- */
    /* failed - deallocate all and return 0     */
    /* ---------------------------------------- */
    if (!m[i]){
      for (j=nrl;j<i;j++) { m[j]+=ncl; delete[] m[j]; };
      m+=nrl; delete[] m;
      return 0;
    }
    m[i] -= ncl;
  }
  return m;
}

void free_dmatrix(double **m,int a, int b) {
    free_dmatrix(m,0,a-1,0,b-1);
}

/*---------------------------------------------------------*/
/* Delete a real matrix                                    */
/*---------------------------------------------------------*/
void free_dmatrix(double **m,int nrl,int nrh,int ncl,int nch)
{ int i;
  for (i=nrh;i>=nrl;i--) { m[i]+=ncl; delete[] m[i]; };
  m+= nrl;
  delete[] m;
  //m=NULL;
}

void print_dmatrix(double **m,int nrl,int nrh,int ncl,int nch)
{ int i,j;
  for (i=nrl;i<nrh;i++) {
    for (j=ncl;j<nch;j++) {
       std::cout << m[i][j] <<"\n";
    }
  }
}

void print_dvector(double *v,int nl,int nh)
{ int i;
  for (i=nl;i<nh;i++) {
     std::cout << v[i] <<"\n";
  }
}


int Retrieve_SymMatrix( FILE *f, MatrixXd &matrix)
{

  /* --------------------------------------------- */
  /* read header                                   */
  /* --------------------------------------------- */
  int      res       =  0;
  int      r         =  0;
  int      c         =  0;
  long int datasize  =  0;
  int      esize     =  0;
  long int data_read =  0;

  data_read += fread(&r ,    sizeof(int), 1, f);
  data_read += fread(&c,     sizeof(int), 1, f);
  data_read += fread(&esize, sizeof(int), 1, f);

  if( data_read != 3 ) {
    std::cout <<"Could not read header!"<< std::endl;
    res = 2;
  }
  if( esize != sizeof(double) ) {
    std::cout << "Stored ElementSize %d does not match expected %d\n" << esize << " "<<sizeof(double)<< std::endl;
    res = 2;
  }
  if ( r < 0 || c < 0 ) {
    std::cout << "Negative number of rows/cols in FILE!\n"<< std::endl;
    res = 2;
  }

  
  if ( res == 0 ) {

    data_read=0;
    datasize = r*(c+1)/2;
    double* vec;
    vec = make_dvector(datasize);
    if ( datasize > 0 )
      data_read=fread(vec,sizeof(double),datasize,f);

    if ( data_read != datasize ) {
      std::cout <<"Unexpected number of elements read in FILE !\n"<< std::endl;
      res = 2;
    }
    //std::cout << "Here is the matrix m:\n"  << std::endl;
    //print_dvector(vec,0 ,datasize);
    int k=0;
    for (int i=0; i<r; i++) {
       for (int j=0; j<=i; j++) {
           matrix(i,j)=vec[k];
           matrix(j,i)=vec[k];
           k++;
       }
    }
    free_dvector(vec, datasize);
  }

  std::cout << "Here is the matrix m:\n"<< matrix  << std::endl;

  return res;
}

int Store_SymMatrix(FILE *f, MatrixXd &matrix) 

{
  if ( f == 0 ) {
    std::cout << "Uninitialized file-pointer in Store FILE !\n"<< std::endl;
    return 2;
  }
  /* --------------------------------------------- */
  /* Initialize                                    */
  /* --------------------------------------------- */
  int        res          = 0;
  int        r            = matrix.rows();
  int        c            = matrix.cols();
  long int   datasize     = r*(c+1)/2;
  int        esize        = sizeof(double);
  long int   data_stored  = 0;


  data_stored += fwrite(&r,     sizeof(int), 1, f);
  data_stored += fwrite(&c,     sizeof(int), 1, f);
  data_stored += fwrite(&esize, sizeof(int), 1, f);

  // Write data (if any)
  if( datasize > 0 ) {

    double* vec;
    vec = make_dvector(datasize);
    int k=0;
    for (int i=0; i<r; i++) {
       for (int j=0; j<=i; j++) {
           vec[k]=matrix(i,j);
           k++;
       }
    }
    //std::cout << "Here is the matrix m:\n"  << std::endl;
    //print_dvector(vec,0 ,datasize);

    data_stored=fwrite(vec, sizeof(double), datasize,f);

    free_dvector(vec, datasize);

  }
  // 3 - magic number, write cols, rows, elementsize
  if (data_stored != datasize+3 ) res = 2;

  return res;
}


int Retrieve_Matrix( FILE *f, MatrixXd &matrix)
{

  /* --------------------------------------------- */
  /* read header                                   */
  /* --------------------------------------------- */
  int      res       =  0;
  int      r         =  0;
  int      c         =  0;
  long int datasize  =  0;
  int      esize     =  0;
  long int data_read =  0;

  data_read += fread(&r ,    sizeof(int), 1, f);
  data_read += fread(&c,     sizeof(int), 1, f);
  data_read += fread(&esize, sizeof(int), 1, f);

  if( data_read != 3 ) {
    std::cout <<"Could not read header!"<< std::endl;
    res = 2;
  }
  if( esize != sizeof(double) ) {
    std::cout << "Stored ElementSize %d does not match expected %d\n" << esize << " "<<sizeof(double)<< std::endl;
    res = 2;
  }
  if ( r < 0 || c < 0 ) {
    std::cout << "Negative number of rows/cols in FILE!\n"<< std::endl;
    res = 2;
  }

  
  if ( res == 0 ) {

    data_read=0;
    datasize = r*c;
    double* vec;
    vec = make_dvector(datasize);
    if ( datasize > 0 )
      data_read=fread(vec,sizeof(double),datasize,f);

    if ( data_read != datasize ) {
      std::cout <<"Unexpected number of elements read in FILE !\n"<< std::endl;
      res = 2;
    }
    //std::cout << "Here is the matrix m:\n"  << std::endl;
    //print_dvector(vec,0 ,datasize);
    int k=0;
    for (int i=0; i<r; i++) {
       for (int j=0; j<c; j++) {
           matrix(i,j)=vec[k];
           k++;
       }
    }
    free_dvector(vec, datasize);
  }

  //std::cout << "Here is the matrix m:\n"<< matrix  << std::endl;

  return res;
}

int Store_Matrix(FILE *f, MatrixXd &matrix) 

{
  if ( f == 0 ) {
    std::cout << "Uninitialized file-pointer in Store FILE !\n"<< std::endl;
    return 2;
  }
  /* --------------------------------------------- */
  /* Initialize                                    */
  /* --------------------------------------------- */
  int        res          = 0;
  int        r            = matrix.rows();
  int        c            = matrix.cols();
  long int   datasize     = r*c;
  int        esize        = sizeof(double);
  long int   data_stored  = 0;


  data_stored += fwrite(&r,     sizeof(int), 1, f);
  data_stored += fwrite(&c,     sizeof(int), 1, f);
  data_stored += fwrite(&esize, sizeof(int), 1, f);

  // Write data (if any)
  if( datasize > 0 ) {

    double* vec;
    vec = make_dvector(datasize);
    int k=0;
    for (int i=0; i<r; i++) {
       for (int j=0; j<c; j++) {
           vec[k]=matrix(i,j);
           k++;
       }
    }
    //std::cout << "Here is the matrix m:\n"  << std::endl;
    //print_dvector(vec,0 ,datasize);

    data_stored=fwrite(vec, sizeof(double), datasize,f);

    free_dvector(vec, datasize);

  }
  // 3 - magic number, write cols, rows, elementsize
  if (data_stored != datasize+3 ) res = 2;

  return res;
}


/*------------------------------------------------------------------------
 Convert a string to uppercase
 ------------------------------------------------------------------------*/
char *Up(char *Result, char *s_) {
  char s[1024];
  int i, FORLIM;
  strcpy(s, s_);
  FORLIM = strlen(s);
  for (i = 0; i < FORLIM; i++)
    s[i] = toupper(s[i]);
  return strcpy(Result, s);
}

char *Lo(char *Result, char *s_) {
  char s[1024];
  int i, FORLIM;
  strcpy(s, s_);
  FORLIM = strlen(s);
  for (i = 0; i < FORLIM; i++)
    s[i] = tolower(s[i]);
  return strcpy(Result, s);
}

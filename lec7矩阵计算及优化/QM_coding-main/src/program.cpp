#include <tools/linear.hpp>
#include <program.hpp>

int main(int argc, char **argv)
{

  program::foo();

  if (argc<2){
    std::cout << "There is no argument\n" << std::endl;
  }
  else {
     for (int i=1; i<argc; i++)  std::cout << argv[i] << std::endl;
  }

  // -----------------  Read Me -------------------------------
  //
  // The program is written by Yang Guo for eduction only.
  // One may use the subroutines in the tools to practise
  // 1. read/write matrix
  // 2. matrix muliplication
  // 3. CSR type sparse matrix
  // 4. sparse matrix multiplication
  //
  // ----------------------------------------------------------

  int nrow, ncol;
  nrow = 10;
  ncol = 10;
  MatrixXd mat = MatrixXd::Random(nrow,ncol);
 
  std::cout << mat << std::endl;

  // open input file
  char file_name[256];
  FILE *input;
  strcpy(file_name, "my_mat.dat");
  input=fopen(file_name, "wb");
  int res = Store_Matrix(input, mat);
  if(res==1) std::cout << "There might be bugs\n" << std::endl;
  fclose(input);
  // -----------------------------------------------------------
  // 
  //            Conventional Matrix
  //
  // 1. Read  Matrix
  // 2. Print Matrix
  // 3. Store Matrix
  // 4. Matrix multiplication
  // ------------------------------------------------------------
  MatrixXd mat2(nrow, ncol);
  input=fopen(file_name, "rb");
  res = Retrieve_Matrix(input, mat2);
  if(res==1) std::cout << "There might be bugs\n" << std::endl;
  fclose(input);
  std::cout << "mat2" << std::endl;
  std::cout << mat2 << std::endl;
  // -----------------------------------------------------------
  //
  //            Sparse Matrix
  //
  // 1. Transform matrix to CSR format
  // 2. CSR matrix multiplication 
  // ------------------------------------------------------------


  return 0;
}



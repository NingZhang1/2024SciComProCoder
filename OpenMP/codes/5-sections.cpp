#include <cstdio>
#include <omp.h> 
int main(int argc, char** argv)
{
#pragma omp parallel sections num_threads(4)
  {
#pragma omp section
    {
      printf("This thread is for graph rendering ...\n");
    }
#pragma omp section
    {
      printf("This thread is for newwork connection ...\n");
    }    
#pragma omp section
    {
      printf("This thread is for numeric calculation ...\n");
    }
#pragma omp section
    {
      printf("This thread is to detect number of threads: %d\n", omp_get_num_threads());
    }    
  }  
  return 0;
}

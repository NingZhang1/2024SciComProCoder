#include <cstdio>
#include <omp.h>
int main(int argc, char** argv)
{
  const int N = 8;
#pragma omp parallel num_threads(4)  
  {
    printf("Thread %d\n", omp_get_thread_num());
#pragma omp barrier
    printf("We are here!\n");
  }
  return 0;
}

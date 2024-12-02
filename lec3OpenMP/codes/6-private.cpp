#include <cstdio>
#include <omp.h> 
int main(int argc, char** argv)
{
  int i = 100;
#pragma omp parallel for private(i)
  for(i = 0; i < 4; ++i)
  {
    printf("In the loop: i = %d\n", i);
  }
  printf("Out of the loop: i = %d\n", i);
return 0;
}

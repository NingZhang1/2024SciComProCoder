#include <cstdio>
#include <cstdlib>
int main(int argc, char** argv)
{
  const int N = 400000;
  double* p = (double*)malloc(N*sizeof(double));
#pragma omp parallel for proc_bind(close)
  for(int i = 0; i < N; ++i)
  {
    p[i] = 0.;
    for(int j = 0; j < N; ++j)
    {
      p[i] += j*1.E-6;
    }
  }
  free(p);
  return 0;
}

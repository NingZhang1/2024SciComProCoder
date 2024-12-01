#include <cstdio>
int main(int argc, char** argv)
{
    const int N = 200000;
    double a[N];
    for(int i = 0; i < N; ++i)
    {
        a[i] = 0.01;
    }
    double c = 0;   
#pragma omp parallel for reduction(+:c) schedule(dynamic)
    for(int i = 0; i < N; ++i)
    {
        for(int j = 0; j < i; ++j)
        {
            c += a[i]*a[j];
        }
    }
    printf("Theoretical value: %.8f.\n", 0.0001*N*(N-1)/2);
    printf("Calculated value:  %.8f\n", c);
    return 0;
}

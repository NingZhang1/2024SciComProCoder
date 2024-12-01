#include <cstdio>
int main(int argc, char** argv)
{
    const int N = 1000;
    double a[N], b[N], c[N];
    // Initialize a and b.
#pragma omp parallel for
    for(int i = 0; i < N; ++i)
    {
        a[i] = i*0.001;
        b[i] = i*0.002;
    } 
    // Calculate c.
#pragma omp parallel for
    for(int i = 0; i < N; ++i)
    {
        c[i] = a[i]*b[i];
    }
    return 0;
}

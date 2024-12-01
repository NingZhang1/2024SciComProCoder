#include <cstdio>

int main(int argc, char** argv)
{
    const int N = 10000;
    double a[N], b[N];
    for(int i = 0; i < N; ++i)
    {
        a[i] = 0.1;
        b[i] = 0.2;
    }
    double c = 0.;
#pragma omp parallel for // reduction(+:c)
    for(int i = 0; i < N; ++i)
    {
        c += a[i]*b[i];
    }
    printf("Theoretical value: 200.\n");
    printf("Calculated value:  %.3f\n", c);
    return 0;
}

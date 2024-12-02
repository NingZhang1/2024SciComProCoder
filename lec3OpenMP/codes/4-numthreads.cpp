#include <cstdio>
#include <omp.h>

int main(int argc, char** argv)
{
    const int N = 8;
    double a[N];
    omp_set_num_threads(4);
    printf("Number of threads: %d\n", omp_get_num_threads()); // Output: 1
#pragma omp parallel for // num_threads(6)
    for(int i = 0; i < N; ++i)
    {
        printf("Number of threads: %d\n", omp_get_num_threads()); // Output: 4
        printf("Thread ID: %d\n", omp_get_thread_num());
        a[i] = i;
    }
    printf("Number of threads: %d\n", omp_get_num_threads()); // Output: 1
    return 0;
}

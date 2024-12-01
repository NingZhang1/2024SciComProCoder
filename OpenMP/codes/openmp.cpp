#include <cstdio>
#include <omp.h>

int main(int argc, char** argv)
{
    omp_set_num_threads(2);
#pragma omp parallel
    {
         printf("Thread ID: %d\n", omp_get_thread_num());
    }
    return 0;
}

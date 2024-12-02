#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <mpi.h>
#include <omp.h>
double f(int i)
{
    double x = 0;
    for(int j = 0; j < 10000; ++j)
    {
        x += 1./(sin(i*j)+2);
    }
    return x;
}
int main(int argc, char** argv)
{
    // Set up environment.
    const int num_threads = atoi(argv[1]);
    omp_set_num_threads(num_threads);
    MPI_Init(&argc, &argv);
    int rank, num_procs;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
    if(rank ==0)
    {
        printf("Number of MPI processes:                  %d\n", num_procs);
        printf("Number of OpenMP threads per MPI process: %d\n", num_threads);
    }
    // Allocate tasks.
    const int N = 1000000;
    double x = 0.;
    const int d = N/num_procs;
    const int i0 = rank*d;
    const int i1 = (rank == num_procs-1) ? (N) : (i0+d);
    // Kernel.
    double s = 0;
#pragma omp parallel for reduction(+:s)
    for(int i = i0; i < i1; ++i)
    {
        s += f(i);
    }
    MPI_Reduce((void*)&s, (void*)&x, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    // Output.
    if(rank ==0)
    {
        printf("Calculated result: %15.8f\n", x);
    }
    MPI_Finalize();
    return 0;
}

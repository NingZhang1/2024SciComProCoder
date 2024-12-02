#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
//for timeing
struct timeval start_time, end_time;
long seconds, microseconds;
double elapsed;
//
//static long num_steps = 1000000000;  // note the max value of int, long int, longlong 
double step; 
int main (int argc, char *argv[]) 
{   long  int i, num_steps; 	 // note int or long int
    int procnum, my_rank, comm_sz;   
    double x, t_pi=0.0 ; 
    double pi; 
	if ( argc== 2){
      num_steps= atoi(argv[1]);
	}else{
    printf("Usage: %s num_steps omp_thread\n\n"
                   " num_steps: the integral steps \n"
                   , argv[0]);
        return 1;
    }
    step = 1.0/(double) num_steps;

    srand(time(0)); // Seed the random number generator
	gettimeofday(&start_time, 0);
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);  
    MPI_Comm_size(MPI_COMM_WORLD, &comm_sz);
	for (i=my_rank;i< num_steps; i=i+comm_sz){
		x = (i+0.5)*step; 
		//printf("x is %2.20f\n",x);
		t_pi = t_pi + (step*4.0)/(1.0+x*x); 
	}
	MPI_Reduce(&t_pi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    if ( my_rank==0){
	gettimeofday(&end_time, 0);
	seconds = end_time.tv_sec - start_time.tv_sec;
	microseconds = end_time.tv_usec - start_time.tv_usec;
	elapsed = seconds + 1e-6 * microseconds;
	printf("pi is %2.20f\n",pi);
        printf(" it takes %f seconds to finish the computation.\n\n", elapsed);
        printf("CHECK  %2.20f    %ld    %f \n", pi ,  num_steps, elapsed);
	}
	MPI_Finalize();
	return 0;
}

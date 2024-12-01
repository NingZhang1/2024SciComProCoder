/*高性能并行课程算例
    PI 计算的MPI 域并行
 V2 点对点通讯版
JGao 2023 09
*/ 
#include<mpi.h>
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
{   long long int i, num_steps; 	 // note int or long int   
     double x, pi=0.0 ;  //sum 
     double sum =0.0; //change here
     double r_sum =0.0;
	  MPI_Status status;      
    MPI_Init(&argc,&argv);
     int id,numprocs,source;
    MPI_Comm_rank(MPI_COMM_WORLD,&id);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);

	if ( argc== 2){
      if(id == 0) {  
      num_steps= atoll(argv[1]);
      }
	}else{
        if(id == 0) {
          printf("Usage: %s num_steps \n\n"
                   " num_steps: the integral steps \n"
                   , argv[0]);
        }
         MPI_Finalize();
        return 1;
    }
    
    if(id == 0) { 
      for (source = 1; source < numprocs; source++) {
        MPI_Send(&num_steps,1, MPI_LONG_LONG_INT, source,99, MPI_COMM_WORLD);
       } 
    } else {
        MPI_Recv(&num_steps,1, MPI_LONG_LONG_INT, 0, 99, MPI_COMM_WORLD,  &status);
      }
    

    srand(time(0)); // Seed the random number generator
	gettimeofday(&start_time, 0);
      
 	  step = 1.0/(double) num_steps; 
    for(i=id; i< num_steps; i=i+numprocs) 
    { 
        x = (i+0.5)*step; 
		sum = sum + 4.0/(1.0+x*x); 
	  } 
    
	 if (id != 0) { 
        MPI_Send(&sum,1, MPI_DOUBLE, 0,99, MPI_COMM_WORLD);
         } else {/* myid == 0 */
            for (source = 1; source < numprocs; source++) {
              MPI_Recv(&r_sum, 1, MPI_DOUBLE, source, 99, MPI_COMM_WORLD,  &status);
              sum=sum+r_sum;
            }
        }
     if(id == 0) {  
       pi = sum*step;
    //   printf("pi is %2.20f\n",pi);
    }
    
    gettimeofday(&end_time, 0);
    seconds = end_time.tv_sec - start_time.tv_sec;
    microseconds = end_time.tv_usec - start_time.tv_usec;
    elapsed = seconds + 1e-6 * microseconds;
    if(id == 0) {
    printf("pi is %2.20f\n",pi);
	  printf(" it takes %f seconds to finish the computation.\n\n", elapsed); 
	  printf("CHECK POINT %2.20f    %lld    %f \n", pi ,  num_steps, elapsed);
    }
    MPI_Finalize();
    return 0 ;
}

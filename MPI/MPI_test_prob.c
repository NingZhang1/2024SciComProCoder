/*mpi_test_prob.c
Example of MPI_Probe

jgao 2023.09
*/
#include <stdio.h>
#include "mpi.h"

int main( int argc, char *argv[] )
{
   int x;
   float y;
    
   int  myid, numprocs;
   MPI_Status status;
   MPI_Init( &argc, &argv );
   MPI_Comm_rank( MPI_COMM_WORLD, &myid );
   MPI_Comm_size( MPI_COMM_WORLD, &numprocs );

   /*test numprocess*/
   if(numprocs < 3 ) 
    {  if (myid ==0) 
        printf("the number of procs is  %d\n Please set at least 3 procs! \n ",  numprocs );
      MPI_Finalize();
      return 0;  }

   if(myid ==0)  
        { x=100;              /*0->2发送一int型数*/
          MPI_Send(&x,1,MPI_INT,2,99,MPI_COMM_WORLD);
        }
   else if(myid == 1)         /*1->2发送一float型数*/
       {y=100.0;
         MPI_Send(&y,1,MPI_FLOAT,2,99,MPI_COMM_WORLD);
       }
   else if(myid == 2)        /* 2 进程接收 */
     for(int i=0;i<2;i++) {
     MPI_Probe(MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&status);  /*Blocking*/
     if (status.MPI_SOURCE == 0)
       { MPI_Recv(&x,1,MPI_INT,0,99,MPI_COMM_WORLD,&status);
        printf("proc 2 get  %d  from proc 0\n", x ); }
     else if(status.MPI_SOURCE == 1)
      { MPI_Recv(&y,1,MPI_FLOAT,1,99,MPI_COMM_WORLD,&status);
       printf("proc 2  get  %f  from proc 1 \n", y );  }
    }

   MPI_Finalize();
   return 0;
}
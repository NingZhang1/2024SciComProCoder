/*mpi_test_wait.c
Example of MPI_ISend and  wait  test
jgao 2023.10
*/
#include <stdio.h>
#include "mpi.h"
#include <unistd.h>

int main( int argc, char *argv[] )
{
   int x,flag,count;
   
    
   int  myid, numprocs;
   MPI_Status status;
   MPI_Request request;

   MPI_Init( &argc, &argv );
   MPI_Comm_rank( MPI_COMM_WORLD, &myid );
   MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
   flag = 0;
   /*test numprocess*/
   if(numprocs < 2 ) 
    {  if (myid ==0) 
        printf("the number of procs is  %d\n Please set at least 2 procs! \n ",  numprocs );
      MPI_Finalize();
      return 0;  }

   if(myid ==0)  
        { x=100;              /*0->1发送一int型数*/
          printf("proc 0 sleep 2 seconds \n"); 
          sleep(2);
          MPI_Isend(&x,1,MPI_INT,1,99,MPI_COMM_WORLD,&request);
          printf("proc 0 pass mpi_isend and sleep 3 seconds\n"); 
          sleep(3);
          MPI_Wait(&request,&status);
        }
   else if(myid == 1)        /* 2 进程接收 */
     {  MPI_Irecv(&x,1,MPI_INT,0,99,MPI_COMM_WORLD,&request);
        printf("proc 1 start recive! \n"); 
        sleep(6);
        MPI_Wait(&request,&status);
        printf("proc 1 finished sleep 6 and revice x \n"); 
        }
    sleep(4);
    MPI_Barrier(MPI_COMM_WORLD);

    if(myid ==0) 
    printf("Section 2 MPI_Test\n");

    x=0;
    if(myid ==0)  
        { x=100;              /*0->1发送一int型数*/
          count=0;
          printf("proc 0 sleep 2 seconds \n"); 
          sleep(2);
          MPI_Isend(&x,1,MPI_INT,1,99,MPI_COMM_WORLD,&request);
          printf("proc 0 pass mpi_isend and start while\n"); 
          //MPI_Test(&request,&flag,&status);
          //printf("proc 0 test flag %d \n",flag);
          while(!flag)
             { count = count+1;
               printf("proc 0 test count %d \n",count);
               MPI_Test(&request,&flag,&status);
             }
          printf("proc 0 finished mpi_isend \n");  
            }
   else if(myid == 1)        /* 2 进程接收 */
     {  
        count = 0;
        MPI_Irecv(&x,1,MPI_INT,0,99,MPI_COMM_WORLD,&request);
        printf("proc 1 start recive\n"); 
        //MPI_Test(&request,&flag,&status);
        //printf("proc 1 test flag %d \n",flag);
        while(!flag)
             { printf("proc 1 test flag in while %d \n",flag);
                count = count+1;
               printf("proc 1 test count %d \n",count);
               MPI_Test(&request,&flag,&status);
             }
        printf("proc 1 finished  revice x \n"); 
        }


   MPI_Finalize();
   return 0;
}
/*Greeting.c
example program of greeting

jgao 2023.09
*/
#include <stdio.h>
#include <string.h>
#include "mpi.h"
int main(int argc, char* argv[])
{
        int numprocs, myid, source;
        MPI_Status status;
        char message[100];
      /*初始化MPI*/
	  MPI_Init(&argc, &argv);
      /*该函数被各进程各调用一次,得到自己的进程rank值*/
	  MPI_Comm_rank(MPI_COMM_WORLD, &myid);
 	 /*该函数被各进程各调用一次,得到进程数*/
	  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

        if (myid != 0) { /* 非 0 号 发送*/
          /*建立消息*/
 	      strcpy(message, "Hello World!");   
          printf( "Greetings from process %d! \n",myid); 
         /* 发送长度取strlen(message)+1,使\0也一同发送出去*/ 	 	
 	      MPI_Send(message,strlen(message)+1, MPI_CHAR, 0,99, MPI_COMM_WORLD);
         } else {/* myid == 0 */
            for (source = 1; source < numprocs; source++) {
              MPI_Recv(message, 100, MPI_CHAR, source, 99, MPI_COMM_WORLD,  &status);
              printf(" process 0 recive %s\n", message);
            }
        }
    MPI_Finalize();
    return 0;
} /* end main */

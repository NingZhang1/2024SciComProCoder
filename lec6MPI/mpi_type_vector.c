/* C Language - Vector Derived Data Type Example
*/
#include "mpi.h"
#include <stdio.h>
#define SIZE 4

int main(int argc, char *argv[])  {
int numtasks, rank, source=0, dest, tag=1, i;
float a[SIZE][SIZE] = 
    {1.0, 2.0, 3.0, 4.0,  
    5.0, 6.0, 7.0, 8.0, 
    9.0, 10.0, 11.0, 12.0,
    13.0, 14.0, 15.0, 16.0};
float b[SIZE]; 

MPI_Status stat;
MPI_Datatype columntype;   // required variable


MPI_Init(&argc,&argv);
MPI_Comm_rank(MPI_COMM_WORLD, &rank);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

// create vector derived data type
MPI_Type_vector(SIZE, 1, SIZE, MPI_FLOAT, &columntype);
MPI_Type_commit(&columntype);

if (numtasks == SIZE) {
    // task 0 sends one element of columntype to all tasks
    if (rank == 0) {
        for (i=0; i<numtasks; i++) 
        MPI_Send(&a[0][i], 1, columntype, i, tag, MPI_COMM_WORLD);
        }

    // all tasks receive columntype data from task 0
    MPI_Recv(b, SIZE, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &stat);
    printf("rank= %d  b= %3.1f %3.1f %3.1f %3.1f\n",
            rank,b[0],b[1],b[2],b[3]);
    }
else
    printf("Must specify %d processors. Terminating.\n",SIZE);

// free datatype when done using it
MPI_Type_free(&columntype);
MPI_Finalize();
return 0;
}
/* example output
rank= 0  b= 1.0 5.0 9.0 13.0
rank= 1  b= 2.0 6.0 10.0 14.0
rank= 2  b= 3.0 7.0 11.0 15.0
rank= 3  b= 4.0 8.0 12.0 16.0
*/
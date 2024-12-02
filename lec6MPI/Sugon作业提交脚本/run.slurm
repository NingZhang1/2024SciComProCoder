#!/bin/bash
#SBATCH -J qe-test
#SBATCH -N 1
#SBATCH --ntasks-per-node=32
#SBATCH --partition=xhhctdnormal



module purge
module load mpi/openmpi/4.1.2-gcc10.2.0
mpicc -o 1 test.c 
mpirun -np 32 -mca btl ^openib ./1 10000000000000

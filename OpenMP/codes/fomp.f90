program main
use omp_lib 
implicit none
   integer, parameter:: N = 100
    integer:: i, s
    call omp_set_num_threads(4)
    s = 0
!$omp parallel do reduction(+:s)
    do i = 1, N
        s = s+i
    enddo
    write (*,*) "sum = ", s
endprogram

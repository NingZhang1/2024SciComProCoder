
program matmultest
  implicit none

  real*8  :: time_start,time_end
  real*8, allocatable :: A(:,:), B(:,:), C(:,:)
  integer :: Ndim, Nblock

  Ndim = 2048
  write(6,'(a,i8)') " Matrix mult Test with Ndim = ", Ndim

  allocate( A(ndim,ndim), B(ndim,ndim), C(ndim,ndim) )

! give A B values with random()
  call cpu_time(time_start)
  call random_number(A)
  call random_number(B)
  call cpu_time(time_end)
  write(6,'(a,f12.5)') " Time used for getting Random A and B matrix : ", time_end-time_start

  ! direct do matmul
  call matmul_direct(A,B,C,ndim)

  ! matmul with nblock = 16
!  call matmul_block(A,B,C,ndim,16)

  ! matmul with nblock = 32
!  call matmul_block(A,B,C,ndim,64)
  call matmul_transA(A,B,C,ndim)

  call matmul_transA_block(A,B,C,ndim,64)

  call matmul_block_handsimd(A,B,C,ndim,64)
  ! call matmul_block_handsimd1(A,B,C,ndim,32)

  call cpu_time(time_start)
  call dgemm('n', 'n', ndim, ndim, ndim, 1.0d0, A, ndim, B, ndim, 0.d0, C, ndim)
  call cpu_time(time_end)
  write(6,'(a,f12.5)') " Dgemm time: ", time_end-time_start

  deallocate( A,B,C )
end program matmultest

subroutine matmul_transA(A,B,C,n)
  implicit none
  integer,  intent(in)  :: n
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  real*8,   allocatable :: At(:,:)
  integer :: i,j,k
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  allocate( At(n,n) )
  do i = 1,n
  do j = 1,n
    At(j,i) = A(i,j)
  enddo
  enddo
  C = 0.d0
  do i = 1, n
  do j = 1, n
  do k = 1, n
    C(j,i) = C(j,i) + At(k,j)*B(k,i)
  enddo
  enddo
  enddo
  deallocate( At )
  call cpu_time(time_end)
  write(6,'(a,f12.5)') " MatMul_transA time : ", time_end-time_start

  return
end subroutine matmul_transA

subroutine matmul_transA_block(A,B,C,n,nblock)
  implicit none
  integer,  intent(in)  :: n,nblock
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  real*8,   allocatable :: At(:,:)
  integer :: i,j,k,i1,j1,k1
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  allocate( At(n,n) )
  do i = 1,n
  do j = 1,n
    At(j,i) = A(i,j)
  enddo
  enddo
  C = 0.d0
  do i1 = 1,n,nblock
  do j1 = 1,n,nblock
  do k1 = 1,n,nblock
  do i = i1, min(i1+nblock-1,n)
  do j = j1, min(j1+nblock-1,n)
  do k = k1, min(k1+nblock-1,n)
    C(j,i) = C(j,i) + A(k,j)*B(k,i)
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  deallocate( At )
  call cpu_time(time_end)
  write(6,'(a,i4,a,f12.5)') " MatMul_transA_block with nblock = ",nblock," time : ", time_end-time_start

  return
end subroutine matmul_transA_block

subroutine matmul_direct(A,B,C,n)
  implicit none
  integer,  intent(in)  :: n
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  integer :: i,j,k
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  C = 0.d0
  do i = 1, n
  do j = 1, n
  do k = 1, n
    C(j,i) = C(j,i) + A(j,k)*B(k,i)
  enddo
  enddo
  enddo
  call cpu_time(time_end)
  write(6,'(a,f12.5)') " MatMul time used : ", time_end-time_start

  return
end subroutine matmul_direct

subroutine matmul_block(A,B,C,n,nblock)
  implicit none
  integer,  intent(in)  :: n,nblock
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  integer :: i,j,k,i1,j1,k1
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  C = 0.d0
  do i1 = 1,n,nblock
  do j1 = 1,n,nblock
  do k1 = 1,n,nblock
  do i = i1, min(i1+nblock-1,n)
  do j = j1, min(j1+nblock-1,n)
  do k = k1, min(k1+nblock-1,n)
    C(j,i) = C(j,i) + A(j,k)*B(k,i)
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  call cpu_time(time_end)
  write(6,'(a,i4,a,f12.5)') " MatMul with nblock = ",nblock,", time = ", time_end-time_start
  return
end subroutine matmul_block

subroutine matmul_block_handsimd(A,B,C,n,nblock)
  implicit none
  integer,  intent(in)  :: n,nblock
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  integer :: i,j,k,i1,j1,k1
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  C = 0.d0
  do i1 = 1,n,nblock
  do j1 = 1,n,nblock
  do k1 = 1,n,nblock
  do i = i1, min(i1+nblock-1,n)
  do j = j1, min(j1+nblock-1,n), 4
  do k = k1, min(k1+nblock-1,n)
    C(j,  i) = C(j,  i) + A(j,  k)*B(k,i)
    C(j+1,i) = C(j+1,i) + A(j+1,k)*B(k,i)
    C(j+2,i) = C(j+2,i) + A(j+2,k)*B(k,i)
    C(j+3,i) = C(j+3,i) + A(j+3,k)*B(k,i)
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  call cpu_time(time_end)
  write(6,'(a,i4,a,f12.5)') " MatMul with nblock = ",nblock,", time = ", time_end-time_start
  return
end subroutine matmul_block_handsimd

subroutine matmul_block_handsimd1(A,B,C,n,nblock)
  implicit none
  integer,  intent(in)  :: n,nblock
  real*8,   intent(in)  :: A(n,n),B(n,n)
  real*8,   intent(out) :: C(n,n)
  integer :: i,j,k,i1,j1,k1
  real*8  :: time_start,time_end
  call cpu_time(time_start)
  C = 0.d0
  do i1 = 1,n,nblock
  do j1 = 1,n,nblock
  do k1 = 1,n,nblock
  do i = i1, min(i1+nblock-1,n)
  do j = j1, min(j1+nblock-1,n)
  do k = k1, min(k1+nblock-1,n), 4
    C(j,i) = C(j,i) + A(j,k  )*B(k,  i) + A(j,k+1)*B(k+1,i) &
                    + A(j,k+2)*B(k+2,i) + A(j,k+3)*B(k+3,i)
  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
  call cpu_time(time_end)
  write(6,'(a,i4,a,f12.5)') " MatMul with simd and nblock = ",nblock,", time = ", time_end-time_start
  return
end subroutine matmul_block_handsimd1

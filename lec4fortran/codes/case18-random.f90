subroutine random_init_array(arr, cols, rows)
implicit none
integer, intent(in):: cols, rows
real(8) :: arr(cols, rows)
integer :: i, j

call random_seed()
do i=1,  rows
    do j=1, cols
        call random_number(arr(j,i))
    enddo
enddo
end subroutine random_init_array

subroutine order_array(arr, num)
    implicit none
    integer, intent(in):: num
    real(8) :: arr(num, num)
    integer:: i, j
    real(8) :: s0
    s0 = 0
    do i=1, num
        do j = 1, num
            s0 = s0 + arr(j, i) * arr(j, i)
        enddo
    enddo

    write(*, *) s0
end subroutine order_array

subroutine disorder_array(arr, num)
    implicit none
    integer, intent(in):: num
    real(8) :: arr(num, num)
    integer:: i, j
    real(8) :: s0
    s0 = 0
    do i=1, num
        do j = 1, num
            s0 = s0 + arr(i, j) * arr(i, j)
        enddo
    enddo

    write(*, *) s0
end subroutine disorder_array

program main
implicit none
real(8), allocatable :: arr(:, :)
integer:: n
real(8) :: t1, t2
n = 10000

allocate(arr(n, n))

call random_init_array(arr, n, n)

call cpu_time(t1)
call order_array(arr, n)
call cpu_time(t2)
write(*, *)"order use time", t2 - t1

call cpu_time(t1)
call disorder_array(arr, n)
call cpu_time(t2)
write(*, *)"order use time", t2 - t1

deallocate(arr)

end program main


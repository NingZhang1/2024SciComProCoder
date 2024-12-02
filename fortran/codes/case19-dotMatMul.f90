
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

subroutine vector_product(N)
    implicit none 
    real(8), allocatable :: vec1(:), vec2(:)
    real(8) :: val
    integer :: i, n
    allocate(vec1(N), vec2(N))
    do i =1, n 
        vec1 = i * 1.0
        vec2 = i * 2.0
    enddo 
    val = dot_product(vec1, vec2)
    write(*, *) val
    deallocate(vec1)
    deallocate(vec2)
end subroutine vector_product

subroutine matrix_matmul(N)
    implicit none 
    integer :: i, n
    real(8), allocatable :: mat1(:, :), mat2(:, :), res(:, :)

    allocate(mat1(N, N), mat2(N, N), res(N, N) )

    res = matmul(mat1, mat2)
    !write(*, *) val
    deallocate(mat1, mat2, res)
end subroutine matrix_matmul

program main 
    implicit none
    integer :: n
    n = 10000
    call vector_product(n)
    call matrix_matmul(n)
end program main
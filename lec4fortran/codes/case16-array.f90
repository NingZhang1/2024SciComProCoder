program main
    implicit none
    integer :: i, j
    ! 1D int array
    integer :: arr1(10)
    ! 2D int array
    integer :: arr2(10, 10)
    integer :: arr3(10)

    !initialize
    arr1 = [1,2,3,4,5,6,7,8,9,10]

    do i=1, 10
        do j=1, 10
            arr2(j, i) = i + j
        enddo
    enddo

    arr3(:) = 0
    arr3(1:5) = 1
    arr3(6:10) = 2

    write(*, *) "arr1:", arr1(1:10:2)
    write(*, *) "arr2:", arr2(:, 1)
    write(*, *) "arr3:", arr3(10:1:-1)
end program main
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

subroutine writeData2File(filename, data, n, m)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(in) :: n, m
    real(8), intent(in) :: data(n, m)

    !### local
    integer :: i, j
    open(unit=999, file=filename)
    write(999, *) n, m
    do i=1, m
        do j = 1, n 
            write(*, *) "readDataFromFile: " , j, i
            write(999, *) data(j, i)
        enddo
    enddo 
    close(999) 
end subroutine writeData2File

subroutine writeData2BinaryFile(filename, data, n, m)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(in) :: n, m
    real(8), intent(in) :: data(n, m)

    !### local
    integer :: i, j
    open(unit=999, file=filename, form="unformatted")
    write(999) n, m
    write(999) data
    close(999)
end subroutine writeData2BinaryFile

subroutine readDataFromFile(filename, data, n, m)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(inout) :: n, m
    real(8), intent(inout) :: data(n, m)

    !### local
    integer :: i, j
    open(unit=999, file=filename)
    read(999, *) n, m
    write(*, *) "readDataFromFile: " , n, m
    do i=1, m
        do j = 1, n 
            write(999, *) data(j, i)
        enddo
    enddo  
    close(999)
end subroutine readDataFromFile

subroutine readDataFromBinaryFile(filename, data, n, m)
    implicit none
    character(*), intent(in) :: filename
    integer, intent(inout) :: n, m
    real(8), intent(inout) :: data(n, m)

    !### local
    integer :: i, j
    open(unit=999, file=filename, form="unformatted")
    read(999) n, m
    read(999) data
    close(999)
end subroutine readDataFromBinaryFile

program main
    implicit none
    character(len = 50):: filename1, filename2
    integer:: n, m
    real(8):: mat1(10, 15), mat2(10, 15)
    call random_init_array(mat1, 10, 15)
    call random_init_array(mat2, 10, 15)

    filename1 = "123.txt"
    call writeData2File(filename1, mat1, 10, 15)
    call readDataFromFile(filename1, mat1, n, m)

    filename2 = "123.bin"
    call writeData2BinaryFile(filename2, mat2, 10, 15)
    call readDataFromBinaryFile(filename2, mat2, n, m)
end program main



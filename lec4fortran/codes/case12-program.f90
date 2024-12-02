program main
    implicit none
    integer:: i, n
    character*50 char

    n = iargc()
    write(*, *) n

    ! c从0开始
    do i=0, n
        call getarg(i, char)
        write(*, *) i, char
    enddo

end program main
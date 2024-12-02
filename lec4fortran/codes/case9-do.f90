program main
    implicit none
    integer:: i

    do i=1, 5
        write(*, *) i
    end do ! enddo is ok

    do i=2, 10, 2
        write(*, *) i
    enddo
end program main
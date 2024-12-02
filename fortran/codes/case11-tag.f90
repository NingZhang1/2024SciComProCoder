program main
    implicit none
    integer:: i, j
    oloop: do i= 1, 10
        iloop: do j=1, 10
            if((j+i)>10) then
                cycle oloop
            endif
            write(*, *) "I=", i, " J=",j,  " Sum=", i+j
        enddo iloop
    enddo oloop
end program main
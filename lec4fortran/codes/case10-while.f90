program main
    integer:: i
    
    i = 1
    do while(i<11)
        print*, i
        i = i + 1
    enddo

    print*, "final i", i

    ! exit
    do i=1, 100
        if(i > 10) then
            exit
        endif
    enddo
    print*, i

    ! cycle
    do i=1, 10
        if(mod(i, 2) == 0) then
            cycle
        endif
    print*, i
    enddo

end program main
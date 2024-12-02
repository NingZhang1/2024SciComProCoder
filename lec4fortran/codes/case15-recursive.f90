
module subs2
implicit none
contains
    recursive integer function fact(n) result(ans)
        implicit none
        integer :: n

        if (n < 0) then
            ans = -1
        else if(n <= 1) then
            ans = 1
        else
            ans = n * fact(n - 1)
        endif
    end function fact
end module 

program main
    use subs2
    implicit none
    integer:: a
    a =  fact(7)
    write(*, *) a
end program main
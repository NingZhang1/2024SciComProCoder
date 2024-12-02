module subs
implicit none
contains
    integer function test(n)
        implicit none
        integer :: n
    end function test

    integer function func_add2(a, b) result(res)
        implicit none
        integer :: a, b
        res = a + b
        write(*, *) a, b
    end function func_add2

    integer function func_add(a, b) 
        implicit none
        integer :: a, b

        func_add = a + b
        write(*, *) a, b
    end function func_add
end module 

program main
    use subs
    implicit none
    integer:: a, b, c, d
    a = 1
    b = 2
    c = func_add(a, b)
    !c = test(a)
    d = func_add2(a, b)
    write(*, *) c, d
end program main
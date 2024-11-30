program main
    implicit none

    integer:: a, b, c

    a = 5
    b = 3
    write(*, *) "a+b = ", a+b
    write(*, *) "a-b = ", a-b
    write(*, *) "a*b = ", a*b
    write(*, *) "a/b = ", a/b
    write(*, *) "a**b = ", a**b

    c= 0
    write(*, *) "a/0= ", a/c

end program main
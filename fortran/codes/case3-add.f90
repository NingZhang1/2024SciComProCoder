subroutine sub_add(a, b)
    implicit none
    ! 局部变量声明
    integer:: a, b
    integer:: c

    c = a + b
    write(*, *) a, b, c
end subroutine sub_add

program main
    implicit none
    integer:: a, b

    a = 1
    b = 3
    ! 调用函数
    call sub_add(a, b)
end program main
subroutine sub_add(a, b)
    implicit none 
    integer:: a
    integer:: b

    b = b + a
    a = 3
end subroutine sub_add

subroutine sub_add2(a, b)
    implicit none 
    integer, intent(in):: a
    integer, intent(out):: b

    b = a + b
    write(*, *) a, b
end subroutine sub_add2

subroutine sub_optional(a, b, c)
    integer, intent(in):: a
    integer, intent(out):: b
    integer, optional :: c
    if(present(c)) then
        c = a + b
        !write(*, *) a, b
    else
        b = a + b
    end if
end subroutine sub_optional

program main
    integer:: a, b, c
    a = 1
    b = 2
    call sub_add2(a, b)

    write(*, *) a, b
    call sub_add(a, b)
    write(*, *) a, b

    !call sub_optional(a, b)
    write(*, *) a, b, c
    call sub_optional(a, b, c)
    write(*, *) a, b, c

end program main
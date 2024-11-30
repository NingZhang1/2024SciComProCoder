module mod_test
    implicit none
    integer:: mod_int = 2
    real :: mod_real = 1.0
end module mod_test

subroutine sub_mod
    use mod_test, only: mod_int, mod_real
    mod_int  = mod_int + 1
    mod_real = mod_real * 2
    write(*, *) mod_int, mod_real
end subroutine sub_mod

program main
    use mod_test
    call sub_mod
end program
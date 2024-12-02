subroutine sub_common1
    implicit none
    real alpha, beta
    common /coeff/ alpha, beta

    write(*, *) "alpha i:", alpha
    write(*, *) "beta i:", beta
    alpha = 0.1
    beta = 0.2
    write(*, *) "alpha", alpha
    write(*, *) "beta", beta

end subroutine sub_common1

subroutine sub_common2
    implicit none
    real alpha, beta
    common /coeff/ alpha, beta

    write(*, *) "alpha i:", alpha
    write(*, *) "beta i:", beta
    alpha = 0.314
    beta = 0.618
    write(*, *) "alpha", alpha
    write(*, *) "beta", beta

end subroutine sub_common2

program main
    implicit none
    !定义 coeff 变量空间
    common /coeff/ alpha, beta
    real alpha, beta

    call sub_common1
    call sub_common2

end program main
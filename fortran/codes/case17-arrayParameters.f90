subroutine arrPara1(arr, n, m)
    implicit none
    integer, intent(in):: n, m
    real(8), intent(inout):: arr(n, m)
    integer:: i, j
    !real*8, intent(in):: arr(n, n)
    arr = 1
end subroutine arrPara1

subroutine arrPara2(arr, n)
    implicit none
    integer, intent(in):: n
    real(8), intent(inout):: arr(*)
    integer:: i, j, m
    arr(1:3) = 2
end subroutine arrPara2

subroutine arrPara3(arr, n)
    implicit none
    real(8), intent(inout):: arr(:, :)
    integer:: i, j, m, n
    arr = 3
end subroutine arrPara3

program main
    implicit none
    integer:: n, m
    real(8):: arr(3, 3)

    call arrPara1(arr, 3, 3)
    Write (*, '(9f3.0)') arr

    call arrPara2(arr, 3)
    Write (*, '(9f3.0)') arr

    !call arrPara3(arr)

end program main
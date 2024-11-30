subroutine sub_save()
    implicit none
    ! 子函数中给定初始值时，会默认成save变量
    integer, save:: saveVar = 0
    integer:: saveVar2 = 0
    integer:: normVar

    normVar = 0

    saveVar = saveVar + 1
    saveVar2 = saveVar2 + 1
    normVar = normVar + 1

    write(*, *) "saveVar: ", saveVar,"saveVar2",saveVar2, "normVar", normVar
end subroutine sub_save

program main
    implicit none
    call sub_save()
    call sub_save()
    call sub_save()
end program main
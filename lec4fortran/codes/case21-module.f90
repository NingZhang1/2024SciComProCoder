module bank
    implicit none
    integer:: money = 0
    private money !声明私有外部不可访问
    public SpendMoney, SaveMoney, Report
contains
    subroutine SpendMoney(num)
        implicit none
        integer:: num 
        money = money - num
    end subroutine

    subroutine SaveMoney(num)
        implicit none
        integer:: num 
        money = money + num
    end subroutine

    subroutine report()
        implicit none
        write(*, *) "now money = ", money
    end subroutine
end module

program main
    use bank
    implicit none
    call SpendMoney(10)
    call Report()
    call SaveMoney(100)
    call Report()
end program
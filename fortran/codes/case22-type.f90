


program main
    implicit none
    type atom
        character(len=2) ele
        real(8) :: x
        real(8) :: y
        real(8) :: z
    end type atom

    type(atom)::atom1
    type(atom)::atom2

    atom1%ele = "O "
    atom1%x = 1.
    atom1%y = 2.
    atom1%z = 3.

    atom2%ele = "Na"
    atom2%x = -1.
    atom2%y = -2.
    atom2%z = -3.
    write(*, *) atom1%ele, atom1%x, atom1%y, atom1%z 
    write(*, *) atom2%ele, atom2%x, atom2%y, atom2%z 
end program 
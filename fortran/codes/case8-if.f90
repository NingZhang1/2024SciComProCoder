program main
    implicit none
    real*8 :: angle

    if (angle < 90.0) then
        write(*, *) "Angle is acute"
    else if(angle < 180) then
        write(*, *) "Angle is obtuse"
    else
        write(*, *) "Angle is reflex"
    end if ! endif is also ok

end program main
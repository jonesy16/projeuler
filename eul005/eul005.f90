!************************************************************
! Project Euler - Problem # 5
!
! Find the smallest number evenly divisible by 1-20
!
! Solved by Eric Jones
!
!************************************************************
program divisibleby
  implicit none
  integer*2 :: divlimit=20
  integer*4 :: numerator, i
  numerator=20
  do
    do i=1, divlimit, 1
      if ( mod(numerator,i) /= 0 ) exit
      if ( i == divlimit) then
        write (*,'(A,I0,A,I0)') "Smallest number divisble by 1-", divlimit, " is ", numerator
        stop
      end if
    end do
    numerator=numerator+20
  end do
end program

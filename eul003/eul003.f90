!************************************************************
! Project Euler - Problem # 3
!
! Find the largest prime factor of 600851475143
!
! Solved by Eric Jones
!
!************************************************************
program largefactor
  implicit none
  integer*8 :: limit=600851475143
  integer*4 :: i
  i=1
  do
    if ( mod(limit,i) == 0 ) then
      limit=limit/i
    end if
    i=i+1
    if ( i >= limit ) exit
  end do
  write (*,'(A,I0,A,I0)') "The largest factor is ", limit
end program

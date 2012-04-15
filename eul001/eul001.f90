!************************************************************
! Project Euler - Problem # 1
!
! Find the sum of all the multiples of 3 or 5, below 1000
!
! Solved by Eric Jones
!
!************************************************************
program multsum
  implicit none
  integer, parameter :: limit=1000
  integer :: multipleSum, i
  do i=3, limit-1, 3
    multipleSum=multipleSum+i
  end do
  do i=5, limit-1, 5
    if ( mod(i,3) == 0 ) cycle
    multipleSum=multipleSum+i
  end do
  write (*,'(A,I0,A,I0)') "The sume of all multiples of 3 or 5 below ", limit, " is ", multipleSum  
end program

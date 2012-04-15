!************************************************************
! Project Euler - Problem # 2
!
! Find the sum of all even-valued Fibonacci terms whose
! value does not exceed 4,000,000.
!
! Solved by Eric Jones
!
!************************************************************
program fibevensum
  implicit none
  integer*4, parameter :: limit=4000000
  integer*8 :: fibSum, fibSeq, fibNum1, fibNum2
  fibNum1=1
  fibNum2=1
  fibSum=0
  do
    fibSeq=fibNum1+fibNum2
    if ( fibSeq > limit ) exit
    if ( mod(fibSeq,2) == 0 ) then
      fibSum=fibSum+fibSeq
    end if
    fibNum1=fibNum2
    fibNum2=fibSeq
  end do
  write (*,'(A,I0,A,I0)') "The sum of the even-valued terms of the Fibonacci sequence below ", limit, " is ", fibSum
end program

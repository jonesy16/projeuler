!************************************************************
! Project Euler - Problem # 14
!
! Based on the following iterative sequence, which starting
! number below 1E6 produces the longest sequence
! 
! n -> n/2 (even #'s)
! n -> 3n+1 (odd #'s)
!
! Solved by Eric Jones
!
!************************************************************
program sequenceLength
  implicit none
  integer*4 :: i, seqLimit=1E6, startNumber, seqLength, maxLength
  integer*8 :: j
  seqLength=0
  maxLength=0
  do i=1, seqLimit-1, 1
    j=i
    seqLength=0
    do
      seqLength=seqLength+1
      if ( j == 1 ) then
        exit
      end if
      if ( mod(j,2) == 0 ) then
        j=j/2
      else
        j=3*j+1
      end if
    end do
    if ( seqLength > maxLength ) then
      maxLength=seqLength
      startNumber=i
    end if
  end do
  print '(A,I0,A,I0)', "Longest chain is ", maxLength, " numbers long and started with ", startNumber
end program


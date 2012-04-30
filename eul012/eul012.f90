!************************************************************
! Project Euler - Problem # 12
!
! Find the first triangle number with 500 divisors
! 
!
! Solved by Eric Jones
!
!************************************************************
program triangledivisor
  implicit none
  integer*8 :: triangleNumber
  integer*4 :: i, j, targetDivisors=500, numDivisors, divLimit, increment
  i=0
  triangleNumber=0
  do
    i=i+1
    triangleNumber=triangleNumber+i
    if ( triangleNumber > 250000 ) then
      divLimit=sqrt(real(triangleNumber))
      numDivisors=2
      if ( MOD(triangleNumber,2) == 0 ) then
        increment=1
      else
        increment=2
      end if
      do j=2, divLimit, increment
        if ( MOD(triangleNumber,j) == 0 ) then
          numDivisors=numDivisors+2
          if ( numDivisors == 500 ) then
            write (*,'(A,I0)') "The first triangle number with 500 divisors is ", triangleNumber
            stop
          end if
        end if
      end do
   end if
  end do
end program


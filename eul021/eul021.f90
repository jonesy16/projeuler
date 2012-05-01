!************************************************************
! Project Euler - Problem # 21
!
! Find the sum of all amicable numbers less than 1000
!
! Solved by Eric Jones
!
!************************************************************
function divisorSum (number)
  implicit none
  integer*8 :: number, divisorSum, k
  divisorSum=1
  do k=2, sqrt(real(number)), 1
    if ( mod(number,k) == 0 ) then
      divisorSum=divisorSum+k
      divisorSum=divisorSum+number/k
    end if
  end do
end function

program amicablesum
  implicit none
  integer*8 :: i, divSum1, divSum2, divisorSum
  integer*8 :: numberSum=0
  logical :: numberTested(1:10000)=.FALSE.
  do i=2, 10000
    if ( numberTested(i) == .FALSE. ) then
      divSum1=divisorSum(i)
      numberTested(i)=.TRUE.
      divSum2=divisorSum(divSum1)
      if ( divSum1 > 10000 .or. divSum2 > 10000 ) cycle
      if ( i == divSum1 ) cycle
      numberTested(divSum1)=.TRUE.
      if ( divSum2 == i ) then
        print *, "Amicable pairs found = ", i, " and ", divSum1
        numberSum=numberSum+i+divSum1
      end if
    end if
  end do
  write (*,'(A,I0)') "The sum of the amicable numbers under 10000 is ", numberSum
end program


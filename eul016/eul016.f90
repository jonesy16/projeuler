!************************************************************
! Project Euler - Problem # 16
!
! Find the sum of the digits in 2^1000
!
! Solved by Eric Jones
!
!************************************************************
program largedigitsum
  implicit none
  integer, parameter :: arraysize=1000000
  integer*4 :: i, numTens, j, maxIndex
  integer*8 :: powerNumber(1:arraysize)
  maxIndex=1
  PowerNumber(:)=0
  PowerNumber(1)=1
  do i=1, 1000
    PowerNumber(:)=PowerNumber(:)*2
    if ( PowerNumber(1) > 100000 .or. i == 1000 ) then
      do j=1, arraysize
        numTens= powerNumber(j)/10
        powerNumber(j)=mod(powerNumber(j),10)
        powerNumber(j+1)=powerNumber(j+1)+numTens
        if (i == maxIndex) then
          maxIndex=maxIndex+log10(real(numTens)) + 1
        end if
        if (i == maxIndex) exit
      end do
    end if
  end do
  print *, "Sum is ", sum(powerNumber(:))
end program

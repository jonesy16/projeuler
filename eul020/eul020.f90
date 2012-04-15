!************************************************************
! Project Euler - Problem # 20
!
! Find the sum of the digits in the number 100! (factorial)
!
! Solved by Eric Jones
!
!************************************************************
program factsummation
  implicit none
  integer*2, parameter :: ARRAYSIZE=1000
  integer*2 :: limit=100, factArray(1:ARRAYSIZE), factSum, i, maxIndex, numTens, j
  integer*8 :: fact
  fact=1
  maxIndex=1
  factArray(:)=0
  factArray(1)=1
  do i=2, limit
    do j=1, maxIndex
      factArray(j)=factArray(j)*i
    end do
    do j=1, ARRAYSIZE, 1
      if ( factArray(j) >= 10 ) then
        numTens= factArray(j)/10
        factArray(j)=mod(factArray(j),10)
        factArray(j+1)=factArray(j+1)+numTens
        if (j == maxIndex) then
          maxIndex=maxIndex+log10(real(numTens)) + 1
        end if
        if (j == maxIndex) exit
      end if
    end do
  end do
  factSum=sum(factArray(:))
  write (*,'(A,I0,A,I0)') "The sum of ", limit, " factorial is ", factSum
end program

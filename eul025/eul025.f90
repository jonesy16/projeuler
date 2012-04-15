!************************************************************
! Project Euler - Problem # 25
!
! Find the first Fibonacci term that is 1000 digits long
!
! Solved by Eric Jones
!
!************************************************************
program panlengthcheck
  implicit none
  ! Assume that the number we find will be less than 100,000 digits long
  ! dataArray(:,1) = first fib. number
  ! dataArray(:,2) = second fib. number
  ! dataArray(:,3) = sum of first and second fib. numbers
  ! dataArray(:,4) = temporary array for math operations
  Integer, Parameter :: ARRAYSIZE=1000
  Integer*4, Dimension (1:ARRAYSIZE,1:3) :: dataArray
  Integer*4 :: seqNumber, i, maxIndex, numTens
  dataArray(:,:)=0
  dataArray(1,1)=1
  dataArray(1,2)=1
  dataArray(1,3)=2
  seqNumber=2
  maxIndex=1
  do
    seqNumber=seqNumber+1
    do i=1, ARRAYSIZE, 1 
      if ( dataArray(i,3) >= 10 ) then
        numTens= dataArray(i,3)/10
        dataArray(i,3)=mod(dataArray(i,3),10)
        dataArray(i+1,3)=dataArray(i+1,3)+numTens
        if (i == maxIndex) then
          maxIndex=maxIndex+log10(real(numTens)) + 1
        end if
        if (i == maxIndex) exit
      end if
    end do
    if ( maxIndex == ARRAYSIZE ) then
      print *, "Fibonacci sequence of ", ARRAYSIZE, " digits found at F_", seqNumber
      stop
    end if
    dataArray(1:maxIndex,1)=dataArray(1:maxIndex,2)
    dataArray(1:maxIndex,2)=dataArray(1:maxIndex,3)
    dataArray(1:maxIndex,3)=sum(dataArray(1:maxIndex,1:2),dim=2)
  end do
end Program

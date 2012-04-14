!************************************************************
! Project Euler - Problem # 104
!
! Find the first number in the Fibonacci sequence
! whose first and last 9 digits are pandigital.
! A pandigital number is a sequence of 9 numbers which
! contain one (1) each of the digits 1-9. E.g., 139726854
!
! Solved by Eric Jones
!
!************************************************************
PROGRAM PANCHECK
  IMPLICIT NONE
  ! Assume that the number we find will be less than 100,000 digits long
  ! dataArray(:,1) = first fib. number
  ! dataArray(:,2) = second fib. number
  ! dataArray(:,3) = sum of first and second fib. numbers
  ! dataArray(:,4) = temporary array for math operations
  Integer, Parameter :: ARRAYSIZE=100000
  Integer*4, Dimension (1:ARRAYSIZE,1:4) :: dataArray
  Integer*4 :: seqNumber, i, j, pandigitSeqCount, maxIndex, loopCount, numTens, forceFullsum
  dataArray(:,:)=0
  dataArray(1,1)=1
  dataArray(1,2)=1
  dataArray(1,3)=2
  seqNumber=2
  maxIndex=10
  forceFullsum=0
  do
    seqNumber=seqNumber+1
    ! The two arrays were summed to form the next Fib. number
    ! Since the arrays were summed, some of the array elements may be
    ! greater than 10, these need to be properly carried over to the next
    ! higher element
    dataArray(2:10,4)=dataArray(1:9,3)/10
    dataArray(1:9,3)=mod(dataArray(1:9,3),10)
    dataArray(1:10,3)=sum(dataArray(1:10,3:4),dim=2)
    if ( dataArray(10,3) > 100000000 ) then
       forceFullsum = 1
    end if
    ! Check last 9 digits for pandigital status
    pandigitSeqCount=0
    if (maxval(dataArray(1:9,3)) == 9 .and. minval(dataArray(1:9,3)) == 1 ) then
      do i=1, 9
        do j=1, 9
          if ( dataArray(j,3) == i ) then
            pandigitSeqCount=pandigitSeqCount+1
            exit
          end if
        end do
      end do
    end if
    if ( pandigitSeqCount == 9 ) then
      write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the last 9 digits"
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
        loopCount=0
      end do
      ! Check first 9 digits for pandigital status
      pandigitSeqCount=0
      if (maxval(dataArray(maxIndex-8:maxIndex,3)) == 9 .and. minval(dataArray(maxIndex-8:maxIndex,3)) == 1 ) then
        do i=1, 9, 1
          do j=maxIndex-8, maxIndex, 1
            if ( dataArray(j,3) == i ) then
              pandigitSeqCount=pandigitSeqCount+1
              exit
            end if
          end do
        end do
      end if
      if ( pandigitSeqCount == 9 ) then
        write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the first and last 9 digits"
        write (*,'(A,I0,A)') " This number is ", maxIndex ," digits long"
        exit
      end if
    else if ( forceFullsum == 1 ) then
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
      forceFullsum=0
    end if
    dataArray(1:maxIndex,1)=dataArray(1:maxIndex,2)
    dataArray(1:maxIndex,2)=dataArray(1:maxIndex,3)
    dataArray(1:maxIndex,3)=sum(dataArray(1:maxIndex,1:2),dim=2)
  end do
end Program

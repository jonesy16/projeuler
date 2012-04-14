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
  Integer, Parameter :: ARRAYSIZE=100000
  Integer*4, Dimension (1:ARRAYSIZE,1:3) :: dataArray
  Integer*4 :: seqNumber, i, j, pandigitSeqCount, maxIndex, loopCount, numTens, forceFullsum
  dataArray(:,:)=0
  dataArray(1,1)=1
  dataArray(1,2)=1
  dataArray(1,3)=2
  seqNumber=2
  maxIndex=1
  forceFullsum=0
  do
    seqNumber=seqNumber+1
    ! The two arrays were summed to form the next Fib. number
    ! Since the arrays were summed, some of the array elements may be
    ! greater than 10, these need to be properly carried over to the next
    ! higher element
    do i=1, 9, 1
      if ( dataArray(i,3) >= 10 ) then
        dataArray(i,3)=dataArray(i,3)-10
        dataArray(i+1,3)=dataArray(i+1,3)+1
        if (i == maxIndex) then
          maxIndex=maxIndex+1
          exit
        end if
      end if
    end do
    if ( dataArray(10,3) > 100000000 ) then
       forceFullsum = 1
    end if
    ! Check last 9 digits for pandigital status
    pandigitSeqCount=0
    do i=1, 9
      do j=1, 9
        if ( dataArray(j,3) == 0 ) then
          exit
        else if ( dataArray(j,3) == i ) then
          pandigitSeqCount=pandigitSeqCount+1
          exit
        end if
      end do
    end do
    if ( pandigitSeqCount == 9 ) then
      write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the last 9 digits"
      ! Now that we know that the first 9 digits are pandigital, we need to
      ! compute our fib. sum
      do i=1, ARRAYSIZE, 1
        if ( dataArray(i,3) >= 10 ) then
          numTens= dataArray(i,3)/10
          dataArray(i,3)=mod(dataArray(i,3),10)
          dataArray(i+1,3)=dataArray(i+1,3)+numTens
          if (i == maxIndex) then
            !print *, "changing max index from ", maxIndex, " to "
            maxIndex=maxIndex+log10(real(numTens)) + 1
            !print *, maxIndex, " based on ", numTens
          end if
          if (i == maxIndex) exit
        end if
        loopCount=0
      end do
      ! Check first 9 digits for pandigital status
      pandigitSeqCount=0
      do i=1, 9, 1
        do j=maxIndex, maxIndex-8, -1
          if ( dataArray(j,3) == 0 ) then
            exit
          else if ( dataArray(j,3) == i ) then
            pandigitSeqCount=pandigitSeqCount+1
            exit
          end if
        end do
      end do
      if ( pandigitSeqCount == 9 ) then
        write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the first and last 9 digits"
        write (*,'(A,I0,A)') " This number is ", maxIndex ," digits long"
        stop
      end if
    else if ( forceFullsum == 1 ) then
      ! Now that we know that the first 9 digits are pandigital, we need to
      ! compute our fib. sum
      do i=1, ARRAYSIZE, 1 
        if ( dataArray(i,3) >= 10 ) then
          numTens= dataArray(i,3)/10
          dataArray(i,3)=mod(dataArray(i,3),10)
          dataArray(i+1,3)=dataArray(i+1,3)+numTens
          if (i == maxIndex) then
            !print *, "changing max index from ", maxIndex, " to "
            maxIndex=maxIndex+log10(real(numTens)) + 1
            !print *, maxIndex, " based on ", numTens
          end if
          if (i == maxIndex) exit
        end if
      end do
      forceFullsum=0
    end if
    dataArray(1:maxIndex,1)=dataArray(1:maxIndex,2)
    dataArray(1:maxIndex,2)=dataArray(1:maxIndex,3)
    dataArray(1:maxIndex,3)=sum(dataArray(1:maxIndex,1:2),dim=2)
    !dataArray(1:9,1)=dataArray(1:9,2)
    !dataArray(1:9,2)=dataArray(1:9,3)
    !dataArray(1:9,3)=sum(dataArray(1:9,1:2),dim=2)
    !if (loopCount == 10) then
      !print *, dataArray(1:maxIndex,1)
      !print *, dataArray(1:maxIndex,2)
      !print *, dataArray(1:maxIndex,3)
    !end if
  end do
end Program

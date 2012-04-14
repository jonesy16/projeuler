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
  Integer, Parameter :: ARRAYSIZE=100000
  Integer*2, Dimension (1:ARRAYSIZE) :: array1,array2,arraySum
  Integer :: seqNumber, i, j, pandigitSeqCount, maxIndex
  array1(:)=0
  array2(:)=0
  arraySum(:)=0
  array1(1)=1
  array2(1)=1
  arraySum(1)=2
  seqNumber=2
  maxIndex=1
  do
    seqNumber=seqNumber+1
    ! The two arrays were summed to form the next Fib. number
    ! Since the arrays were summed, some of the array elements may be
    ! greater than 10, these need to be properly carried over to the next
    ! higher element
    do i=1, ARRAYSIZE, 1
      if ( arraySum(i) >= 10 ) Then
        arraySum(i)=arraySum(i)-10
        arraySum(i+1)=arraySum(i+1)+1
        ! If we've already looped through our Fib. number size, we can quit
        ! But first, if the last numbers we summed were greater than 10
        ! we need to increase our max array size index
        if (i == maxIndex) Then
          maxIndex=maxIndex+1
          exit
        end if
      end if
    end do
    ! Check last 9 digits for pandigital status
    pandigitSeqCount=0
    do i=1, 9
      do j=1, 9
        if ( arraySum(j) == 0 ) Then
          exit
        else if ( arraySum(j) == i ) Then
          pandigitSeqCount=pandigitSeqCount+1
          exit
        end if
      end do
    end do
    if ( pandigitSeqCount == 9 ) Then
      write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the last 9 digits"
      ! Check first 9 digits for pandigital status
      pandigitSeqCount=0
      do i=1, 9, 1
        do j=maxIndex, maxIndex-8, -1
          if ( arraySum(j) == 0 ) Then
            exit
          else if ( arraySum(j) == i ) Then
            pandigitSeqCount=pandigitSeqCount+1
            exit
          end if
        end do
      end do
      if ( pandigitSeqCount == 9 ) Then
        write (*,'(A,I0,A)') " F_", seqNumber, " contains a pandigital number in the first and last 9 digits"
        write (*,'(A,I0,A)') " This number is ", maxIndex ," digits long"
        exit
      end if
    end if
  array1(1:maxIndex)=array2(1:maxIndex)
  array2(1:maxIndex)=arraySum(1:maxIndex)
  arraySum(1:maxIndex)=array1(1:maxIndex)+array2(1:maxIndex)
  end do
end Program

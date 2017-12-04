program day1
  implicit none

  character(len=2000000) :: input

  open(1,file='input.txt')
  read(1,'(a)') input
  close(1)

  write(*,'(a,i0)') 'Part1: ',getsum(trim(input),1)
  write(*,'(a,i0)') 'Part2: ',getsum(trim(input),len(trim(input))/2)

contains
  function getsum(str,offset) result (answer)
    character(len=*) :: str
    integer :: offset,answer,i,lookup(len(str))
    lookup(1:offset) = (/(i,i=len(str)+1-offset,len(str))/)
    lookup(offset+1:len(str)) = (/(i,i=1,len(str)-offset)/)
    answer=0
    do i=1,len(str)
       if (str(i:i)==str(lookup(i):lookup(i))) then
          answer = answer+iachar(str(i:i))-48
       end if
    end do
  end function getsum

end program day1

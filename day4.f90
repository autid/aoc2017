program day4
  integer, dimension(512) :: wordcount
  character(len=150) :: input
  character(len=20),dimension(:),allocatable :: phrase
  integer, dimension(:,:),allocatable :: lettercount
  integer ::i,j,k,part1,part2
  
  part1=0
  part2=0
  
  open(1,file='input.txt')
  !determine words per passphrase
  do i=1,512
     read(1,'(a)') input
     wordcount(i)=1
     do j=1,len(trim(input))
        if (input(j:j)==' ') wordcount(i)=wordcount(i)+1
     end do
  end do
  
  rewind(1)
  
  outer: do i=1,512
     allocate(phrase(wordcount(i)))
     read(1,*) phrase
     
     !check for identical words
     do j=1,wordcount(i)-1
        do k=j+1,wordcount(i)
           if (phrase(j)==phrase(k)) then
              deallocate(phrase)
              cycle outer
           end if
        end do
     end do
     
     part1=part1+1

     !make a 26 element array for each word containing the number of each letter present
     allocate(lettercount(97:122,wordcount(i)))
     lettercount=0
     do j=1,wordcount(i)
        input=phrase(j)
        do k=1,len(trim(input))
           lettercount(iachar(input(k:k)),j)=lettercount(iachar(input(k:k)),j)+1
        end do
     end do
     
     !compare letter counts
     do j=1,wordcount(i)-1
        do k=j+1,wordcount(i)
           if (all(lettercount(:,j).eq.lettercount(:,k))) then
              deallocate(phrase)
              deallocate(lettercount)
              cycle outer
           end if
        end do
     end do
     part2=part2+1
     deallocate(lettercount)
     deallocate(phrase)
     
  end do outer

  close(1)
  write(*,*) part1
  write(*,*) part2

end program day4

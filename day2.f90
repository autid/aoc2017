program day2
  integer :: input(16,16), checksum=0, divtotal=0, i, j, k

  open(1,file = 'input.txt')
  read(1,*) input
  close(1)

  do k=1,16
     checksum = checksum + maxval(input(:,k)) - minval(input(:,k))
     do i=1,16
        do j=1,16
           if ((modulo(input(i,k),input(j,k))==0) .and. (i .ne. j)) divtotal=divtotal+input(i,k)/input(j,k)
        end do
     end do
  end do
  write(*,*) checksum, divtotal
end program day2

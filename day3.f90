program day3
  integer, parameter :: puzinput = 368078
  integer,parameter :: sidelength = ceiling(sqrt(real(puzinput)))+2
  integer(8) :: grid(sidelength,sidelength)
  integer :: x,y,xinit,yinit,count,i
  real :: step
  complex :: direction=(1,0),position
  logical :: part2 = .True.

  xinit = sidelength/2+1
  yinit = sidelength/2+1
  position = cmplx(xinit,yinit)
  grid(xinit,yinit)=1
  step=1.0
  count=1
  outer:do
     do i=1,floor(step)
        position = position+direction
        x = int(real(position))
        y = int(aimag(position))
        grid(x,y) = sum(grid(x-1:x+1,y-1:y+1))
        count = count+1
        if (count==puzinput) exit outer
     end do
     step = step+0.5
     direction = direction*cmplx(0,-1)
  end do outer

  write(*,*) 'Part1: ', abs(x-xinit)+abs(y-yinit)
  write(*,*) 'Part2: ', minval(grid,mask=grid>puzinput)
end program day3

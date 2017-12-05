program day5
  integer :: instruction=1,jumps(1052),jump,steps=0


  open(1,file='input.txt')
  read(1,*) jumps
  
  
  do while (instruction>0 .and.instruction<=1052)
     jump=jumps(instruction)
     jumps(instruction) = jumps(instruction)+1
     instruction=instruction+jump
     steps = steps+1
  end do

  write(*,'(a,i0)') 'Part1: ',steps

  steps=0
  instruction=1
  rewind(1)
  read(1,*) jumps

  do while (instruction>0 .and.instruction<=1052)
     jump=jumps(instruction)
     if (jump>=3) then
        jumps(instruction) = jumps(instruction)-1
     else
        jumps(instruction) = jumps(instruction)+1
     end if
     instruction=instruction+jump
     steps = steps+1
  end do

  write(*,'(a,i0)') 'Part2: ',steps
  close(1)
  
end program day5


	program recurrentes

	implicit none
	character *80 input1, input2, output
	integer i,j,k,l,n, T(9999), R(99,9999), nR, rc, nnR(99), difer
	logical ext

	write(*,*) 'Input file (recurrentes): '
	read(*,*) input1
	write(*,*) 'Input file (recurrentes + cuasiestacionarios): '
	read(*,*) input2
	write(*,*) 'Output file: '
	read(*,*) output


	open(90,file=output,status='unknown')
	write(90,'(A1,A)') '#','  R(0)_C(1)  Num_nodes    Nodes'

	open(10,file=input1,status='old')
	read(10,*)
	nR=0
	do
	  read(10,*,end=50) n,(R(nR+1,i),i=1,n)
	  nnR(nR+1)=n
	  write(90,'(2I9,A,$)') 0,n,'      '
	  do j=1,n
	    write(90,'(I6,$)') R(nR+1,j)
	  enddo
	  write(90,*)
	  nR=nR+1
	enddo
 50	continue
	close(10)
	print*,'Número de recurrentes: ',nR


	!inquire(file=input2,exist=ext)
	!if (ext.eq.'TRUE') then
	 open(11,file=input2,status='old')
	 read(11,*)
	 do
	  read(11,*,end=51) n,(T(i),i=1,n)
	  rc=1
	  difer=1
	  do i=1,nR
	    do k=1,nnR(i)
		do l=1,n
		  if(R(i,k).eq.T(l)) then
			difer=0
			exit
		  endif
		enddo
	    enddo
	  enddo
	  if(difer.eq.1) then
	  	write(90,'(2I9,A,$)') 1,n,'      '
	  	do j=1,n
	    	  write(90,'(I6,$)') T(j)
	  	enddo
	  	write(90,*)
	  endif
	 enddo
 51	 continue
	 close(11)
	!endif

	close(90)

	stop
	end

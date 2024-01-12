
	program tocapicues_cuasi

	implicit none
	character *80 input, output, inpig
	integer i,j, T(9999,9999), n, nig, nodei(9999)

	write(*,*) 'All-links input file: '
	read(*,*) input
	write(*,*) 'Output file: '
	read(*,*) output
	write(*,*) 'Input file with nodes to ignore: '
	read(*,*) inpig

	open(11,file=inpig,status='old')
	read(11,*)
	nig=0
	do
	  read(11,*,end=51) n,(nodei(nig+i),i=1,n)
	  nig=nig+n
	enddo
 51	continue
	print*,'Number of nodes to ignore: ',nig 
	print*,(nodei(i),i=1,nig)

	open(10,file=input,status='old')
	read(10,*)
	T=0
	n=0
	do
	  read(10,*,end=50) i,j,j
	  T(i,j)=T(i,j)+1
	  if(i.gt.n) n=i
	enddo
 50	continue
	close(10)

	do i=1,n
	  T(i,i)=T(i,i)+1
	enddo

	do i=1,nig
	  do j=1,n
	    T(nodei(i),j)=0
	    T(j,nodei(i))=0
	  enddo
	enddo

	open(90,file=output,status='unknown')
	do i=1,n
	  do j=1,n
	    if(T(i,j).gt.0) write(90,'(2I8)') i,j
	  enddo
	enddo
	close(90)

	stop
	end

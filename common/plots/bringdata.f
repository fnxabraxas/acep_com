	program bringdata

	implicit none
	integer, parameter :: nSt=18
	character*80 inpX,inpR,outM
	real t,pR(999),pNR(999,9999),X(9999,nSt)
	integer nx,nR,nNR(999),nodNR(999,9999),i,j,k,maxnNR 


	pR=0.
	pNR=0.
	X=0.
	nx=0
	nR=0
	nNR=0
	nodNR=0
	maxnNR=0

	print*,'Recurrentes_estac input file: '
	read(*,*) inpR
	print*,'Xeq input file: '
	read(*,*) inpX
	print*,'Matlab format output file: '
	read(*,*) outM

	open(10,file=inpX,status='old')
	read(10,*)
	nx=0
	do
	  nx=nx+1
	  read(10,*,end=50) t,t,(X(nx,k),k=1,nSt)
	enddo
 50	continue
	nx=nx-1
	close(10)

	nodNR=0
	open(10,file=inpR,status='old')
	read(10,*)
	read(10,*)
	nR=0
	do
	  nR=nR+1
	  read(10,*,end=60) t,pR(nR),nNR(nR)
	  if(t.gt.0) exit
	  do i=1,nNR(nR)
	    read(10,*) nodNR(nR,i), pNR(nR,i)
	  enddo
	enddo
 60	continue
	nR=nR-1
	close(10)

	maxnNR=maxval(nNR)
	open(90,file=outM,status='unknown')
	do i=1,nR
	  write(90,'(I8,E12.4,18I8,A,$)') nNR(i),pR(i),(0,k=1,nSt),'  '
	enddo
	write(90,*)
	do j=1,maxnNR
	  do i=1,nR
	    if(nodNR(i,j).ne.0) then
		write(90,'(I8,E12.4,18F8.4,A,$)')
     &            nodNR(i,j), pNR(i,j), (X(nodNR(i,j),k),k=1,nSt),'  '
	    else
		write(90,'(I8,I12,18I8,A,$)') (0,k=1,nSt+2),'  '
	    endif
	  enddo
	  write(90,*)
	enddo
	close(90)

	stop
	end



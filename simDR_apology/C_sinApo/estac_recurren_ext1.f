
	program estac_recurren_ext1

	implicit none
	include "param_o.h"
	character *80 inpX, inpE, output
	integer i,j,k,l,n,m,ir, S(nSb,4)
	double precision Xeq(99999,nSb),t,p,Pmix(4)

	print*,'Xeq input file: '
	read(*,*) inpX
	print*,'Estac_recurren input file: '
	read(*,*) inpE
	print*,'Output file: '
	read(*,*) output

	open(10,file=inpX,status='old')
	read(10,*)
	n=0
	do
	  n=n+1
	  read(10,*,end=50) t,t,(Xeq(n,k),k=1,nSb)
	enddo
  50	continue
	n=n-1
	close(10)

	m=0
	do i=0,1
	do j=0,1
	do k=0,1
	do l=0,1
	  m=m+1
	  S(m,1)=i
	  S(m,2)=j
	  S(m,3)=k
	  S(m,4)=l
	enddo
	enddo
	enddo	
	enddo


	open(10,file=inpE,status='old')
	read(10,*)
	read(10,*)
	open(90,file=output,status='unknown')
	write(90,'(A1,A)') '#','  R(0)_C(1)   Prob   Num_nodes'
	write(90,'(A1,A)') '#',
     +		'  Node   Prob      P(CC)  P(CD)  P(DC)  P(DD)'	
	do
	  read(10,*,end=60) j,p,m
	  write(90,'(I3,F12.4,I6)') j,p,m
	  do ir=1,m
	    read(10,*,end=60) i,p
	    Pmix=0.
	    do k=1,nSb
	      if(Xeq(i,k).gt.0) then
		do j=1,4
		  Pmix(j)=Pmix(j)+S(k,j)*Xeq(i,k)
		enddo
	      endif
	    enddo
	    write(90,'(I8,E16.4,A,4F8.4)') i,p,'        ',(Pmix(j),j=1,4)
	  enddo
	enddo
  60	continue
	close(90)
	close(10)

	stop
	end


	program markov

	implicit none
	character*80 inplinks, inpXeq, output
	integer i,j

	double precision T(999,999)
	integer nT
	common /markovchain/ T, nT 

	print*,'All-links file: '
	read(*,*) inplinks
c	print*,'Xeq file: '
c	read(*,*) inpXeq
	print*,'Markov-chain output file: '
	read(*,*) output

	call buildchain(inplinks) !,inpXeq)

	open(90,file=output,status='UNKNOWN')
	write(90,'(A1,$)') '#'
	do i=1,nT
	  write(90,'(I12,$)') i
	enddo
	write(90,*)
	do i=1,nT
	  write(90,'(A1,$)'), ' '
	  do j=1,nT
	    write(90,'(E25.16,$)') T(i,j)
	  enddo
	  write(90,*)
	enddo
	close(90)

	stop
	end

	

	subroutine buildchain(inplinks) !,inpXeq)

	implicit none
	include "param_o.h"
	character*80 inplinks, inpXeq

	integer nini,ninv,nend,node,nodebin,k,nxeq,i,j
	double precision prob, errprob, xeq(nSb), suma

	double precision T(999,999)
	integer nT
	common /markovchain/ T, nT 

	T=0.d0
	nT=0

	open(10,file=inplinks,status='OLD')
	read(10,*)
	do
	  read(10,*,end=50) nini,ninv,nend,prob 
	  if(nini.ne.nend) T(nini,nend)=T(nini,nend)+prob  ! notación matemática
	  if(nend.gt.nT) nT=nend
	enddo
 50	continue
	close(10)

c	open(11,file=inpXeq,status='OLD')
c	read(11,*)
c	do
c	  read(11,*,end=51) node,nodebin,(xeq(k),k=1,16)
c	  nxeq=0
c	  do k=1,16
c	    if(xeq(k).gt.0) nxeq=nxeq+1 ! añade sus propias especies
c	  enddo	
c	  T(node,node)=T(node,node)+nxeq
c	enddo
c 51	continue
c	close(11)
c	nT=node
	
	print*,'Number of nodes: ',nT

	do i=1,nT
	  suma=0
	  do j=1,nT
	    T(i,j)=T(i,j)/(nSb*1.d0)	! normaliza entre las posibles invasiones
	    suma=suma+T(i,j)
	  enddo
	  T(i,i)=-suma
	  if(suma.eq.0) print*,'Nodo absorbente: ',i
	enddo

	return
	end


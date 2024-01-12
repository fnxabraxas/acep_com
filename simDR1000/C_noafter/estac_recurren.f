
	program estac_recurren

	implicit none
	integer, parameter :: Nnmax=999,NRmax=99 
	integer i,j,k, nnodes,R(NRmax,Nnmax),nNR(NRmax),nR,nRt,nT, errorf,
     +				is(NRmax)
	character*80 inpQ,inpR,output
	double precision P(Nnmax,Nnmax),V(Nnmax,Nnmax),b(Nnmax),
     +       Vt(Nnmax,Nnmax),Vinv(Nnmax,Nnmax),probs(Nnmax), ps(NRmax)


	print*,'Q Markov matrix file: '
	read(*,*) inpQ
	print*,'Recurrentes file: '
	read(*,*) inpR
	print*,'Output file: '
	read(*,*) output

	P=0.d0
	b=0.d0
	V=0.d0
	Vinv=0.d0
	Vt=0.d0
	probs=0.d0
	ps=0.d0
	open(10,file=inpQ,status='old')
	read(10,*)
	nnodes=0
	do
	  read(10,*,end=50)
	  nnodes=nnodes+1
	enddo
 50	continue
	close(10)
	open(10,file=inpQ,status='old')
	read(10,*)
	i=0
	do
	  i=i+1
	  read(10,*,end=51) (P(i,j),j=1,nnodes)
	enddo
 51	continue
	close(10)
	open(10,file=inpR,status='old')
	read(10,*)
	nR=0
	nRt=0
	do
	  read(10,*,end=52) is(nR+1),ps(nR+1),nNR(nR+1),
     +				  (R(nR+1,j),j=1,nNR(nR+1))
	  nR=nR+1 !if(is(nR+1).eq.0) nR=nR+1
	enddo
 52     continue	
	close(10)
	do i=1,nR
	  nRt=nRt+nNR(i)
	enddo
	nT=nnodes-nRt
	print*,'Núm conjuntos a analizar: ',nR
	print*,'Núm nodos en analiz,transit,tot: ',nRt,nT,nnodes

	open(90,file=output,status='unknown')
	write(90,'(A1,A)') '#','  R(0)_C(1)   Prob   Num_nodes'
	write(90,'(A1,A)') '#','  Node   Prob'
	do i=1,nR
		!print*,i
	  V=0.
	  do j=1,nNR(i)
	    do k=1,nNR(i)
		V(j,k)=P(R(i,j),R(i,k))
	    enddo
	  enddo

	  Vt=0.
	  do j=1,nNR(i)
	    do k=1,nNR(i)
		Vt(j,k)=V(k,j)
	    enddo
	  enddo

	  do j=1,nNR(i)
	        Vt(1,j)=1.d0
		b(j)=0.d0
	  enddo
	  b(1)=1

	  Vinv=0.
	  call FINDInv(Vt,Vinv,nNR(i),Nnmax,errorf)
	  probs=matmul(Vinv,b)

	  write(90,'(I3,F12.4,I6)') is(i),ps(i),nNR(i)
	  do k=1,nNR(i)
	    write(90,'(I8,E16.4)') R(i,k), probs(k)
	  enddo

	enddo



	stop
	end






        SUBROUTINE FINDInv(matrix, inverse, n,nmax, errorflag)
!Subroutine to find the inverse of a square matrix
!Author : Louisda16th a.k.a Ashwith J. Rego
!Reference : Algorithm has been well explained in:
!http://math.uww.edu/~mcfarlat/inverse.htm           
!http://www.tutor.ms.unimelb.edu.au/matrix/matrix_inverse.html
	IMPLICIT NONE
	!Declarations
	INTEGER, INTENT(IN) :: n,nmax
	INTEGER, INTENT(OUT) :: errorflag  !Return error status. -1 for error, 0 for normal
	double precision, INTENT(IN), DIMENSION(nmax,nmax) :: matrix  !Input matrix
	double precision, INTENT(OUT), DIMENSION(nmax,nmax) :: inverse !Inverted matrix
	
	LOGICAL :: FLAG = .TRUE.
	INTEGER :: i, j, k, l
	double precision :: m
	double precision, DIMENSION(n,2*n) :: augmatrix !augmented matrix
		
	!Augment input matrix with an identity matrix
	DO i = 1, n
	  DO j = 1, 2*n
	  	IF (j <= n ) THEN
	  	  augmatrix(i,j) = matrix(i,j)
	  	ELSE IF ((i+n) == j) THEN
	  	  augmatrix(i,j) = 1
	  	Else
	  	  augmatrix(i,j) = 0
	  	ENDIF
	  END DO
	END DO

	!Reduce augmented matrix to upper traingular form
	DO k =1, n-1
	  IF (augmatrix(k,k) == 0) THEN
	  	FLAG = .FALSE.
	  	DO i = k+1, n
	  	  IF (augmatrix(i,k) /= 0) THEN
	  	  	DO j = 1,2*n
	  	  	  augmatrix(k,j) = augmatrix(k,j)+augmatrix(i,j)
	  	  	END DO
	  	  	FLAG = .TRUE.
	  	  	EXIT
	  	  ENDIF
	  	  IF (FLAG .EQV. .FALSE.) THEN
	  	  	PRINT*, "-Matrix is non - invertible"
	  	  	inverse = 0
	  	  	errorflag = -1
	  	  	return
	  	  ENDIF
	  	END DO
	  ENDIF
	  DO j = k+1, n	  	
	  	m = augmatrix(j,k)/augmatrix(k,k)
	  	DO i = k, 2*n
	  	  augmatrix(j,i) = augmatrix(j,i) - m*augmatrix(k,i)
	  	END DO
	  END DO
	END DO
	
	!Test for invertibility
	DO i = 1, n
	  IF (augmatrix(i,i) == 0) THEN
	  	PRINT*, "Matrix is non - invertible"
	  	inverse = 0
	  	errorflag = -1
	  	return
	  ENDIF
	END DO
	
	!Make diagonal elements as 1
	DO i = 1 , n
	  m = augmatrix(i,i)
	  DO j = i , (2 * n)	  	  
	  	   augmatrix(i,j) = (augmatrix(i,j) / m)
	  END DO
	END DO
	
	!Reduced right side half of augmented matrix to identity matrix
	DO k = n-1, 1, -1
	  DO i =1, k
	  m = augmatrix(i,k+1)
	  	DO j = k, (2*n)
	  	  augmatrix(i,j) = augmatrix(i,j) -augmatrix(k+1,j) * m
	  	END DO
	  END DO
	END DO	  	  
	
	!store answer
	DO i =1, n
	  DO j = 1, n
	  	inverse(i,j) = augmatrix(i,j+n)
	  END DO
	END DO
	errorflag = 0

      END SUBROUTINE FINDinv


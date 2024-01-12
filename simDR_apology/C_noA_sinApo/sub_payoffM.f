
	subroutine 
     + anaPAY(pay1,pay2,is1,is2,coop1,coop2,def1,def2,ccm,timeC,com,TRE)
c	TRE: 00 (strict), 10 (basic), 01(antibasic), 11(relax)
	implicit none
	include "common.h"
	double precision pay1, pay2, coop1,coop2,def1,def2,ccm,timeC
	integer is1,is2,TRE
	integer i,j, irea, com
	double precision MC(32,32),Wd1,Wd2,Wd2t,payMATOUT(32,32),nRC,
     +		coopd1,coopd2,MCC(32,32),ccd,hca1,hca2,hcca,ga,ha1,ha2

	if((TRE.ne.00).and.(TRE.ne.10).and.(TRE.ne.01).and.(TRE.ne.11))
     +		stop 'Error in TRE'

	timeC=0.d0
	ccm=0.d0
	coop1=0.d0
	coop2=0.d0
	def1=0.d0
	def2=0.d0
	if( (prop(is1).eq.1).and.(prop(is2).eq.1) ) then
	  pay1=-epsi/2
	  pay2=-epsi/2
	  com=1
	elseif( (prop(is1).eq.1).and.(accep(is2).eq.1) ) then
	  pay1=-epsi
	  pay2=0
	  com=1
	elseif( (prop(is2).eq.1).and.(accep(is1).eq.1) ) then
	  pay2=-epsi
	  pay1=0
	  com=1
	elseif( (prop(is2).eq.1).or.(prop(is1).eq.1) ) then
	  pay1=0
	  pay2=0
	  if((TRE.eq.00).or.(TRE.eq.01)) com=-1  ! they do not play if a proposal is rejected
	  if((TRE.eq.10).or.(TRE.eq.11)) com=0  ! they play even if the proposal is rejected
	else
	  pay1=0
	  pay2=0
	  com=0
	endif

	call payOUT(MC,MCC,payMATOUT,b,c,e,w)
	do i=1,32
	do j=1,32
	  if ((MC(i,j).lt.0.d0).or.(MC(i,j).gt.1.d0)) then
	    print*,i,j,MC(i,j)
	    stop 'Error'
	  endif
	enddo
	enddo
	if (com.eq.0) then		
	  pay1=(1.d0-w)*pay1 + payMATOUT(Pout(is1)+1,Pout(is2)+1) 
	  pay2=(1.d0-w)*pay2 + payMATOUT(Pout(is2)+1,Pout(is1)+1)
	  coop1=MC(Pout(is1)+1,Pout(is2)+1) 
	  coop2=MC(Pout(is2)+1,Pout(is1)+1) 
	  ccm=MCC(Pout(is1)+1,Pout(is2)+1) 
	  def1=1.d0-coop1
	  def2=1.d0-coop2
	elseif (com.eq.1) then
	  if((Pin(is1).eq.1).and.(Pin(is2).eq.1)) then
	    ga=(1.d0-e)**2.d0+fa(is2,is1)*fa(is1,is2)*e**2
     +				+e*(1.d0-e)*(fa(is1,is2)+fa(is2,is1))
	    ha1=(b-c)*(1.d0-e)**2.d0
     +		+e*(1.d0-e)*((b-gam)*fa(is2,is1)+(gam-c)*fa(is1,is2))
	    ha2=(b-c)*(1.d0-e)**2.d0
     +		+e*(1.d0-e)*((gam-c)*fa(is2,is1)+(b-gam)*fa(is1,is2))
	    hca1=(1.d0-e)**2.d0+e*(1.d0-e)*fa(is1,is2)
	    hca2=(1.d0-e)**2.d0+e*(1.d0-e)*fa(is2,is1)
	    hcca=(1.d0-e)**2.d0
	    Wd1=(b-c)*(1.d0-e) -(delta-gam)*e*(fa(is1,is2)-fa(is2,is1))
     +								 -ha1*w
	    wd2=(b-c)*(1.d0-e) +(delta-gam)*e*(fa(is1,is2)-fa(is2,is1))
     +								 -ha2*w
	    coopd1=((1.d0-e)**2.d0)*(1.d0-w) + e*(1.d0-e)
	    coopd2=coopd1
	    ccd=((1.d0-e)**2.d0)*(1.d0-w)
	  elseif((Pin(is1).eq.0).and.(Pin(is2).eq.0)) then
	    ga=e**2.d0+fa(is2,is1)*fa(is1,is2)*(1.d0-e)**2
     +				+e*(1.d0-e)*(fa(is1,is2)+fa(is2,is1))
	    ha1=(b-c)*e**2.d0
     +		+e*(1.d0-e)*((b-gam)*fa(is2,is1)+(gam-c)*fa(is1,is2))
	    ha2=(b-c)*e**2.d0
     +		+e*(1.d0-e)*((gam-c)*fa(is2,is1)+(b-gam)*fa(is1,is2))
	    hca1=e**2.d0+e*(1.d0-e)*fa(is1,is2)
	    hca2=e**2.d0+e*(1.d0-e)*fa(is2,is1)
	    hcca=e**2.d0
	    Wd1=(b-c)*e -(delta-gam)*(1.d0-e)*(fa(is1,is2)-fa(is2,is1))
     +								 -ha1*w
	    wd2=(b-c)*e +(delta-gam)*(1.d0-e)*(fa(is1,is2)-fa(is2,is1))
     +								 -ha2*w
	    coopd1=e
	    coopd2=coopd1
	    ccd=e
	  else
	    ga=fa(is2,is1)*e**2.d0+fa(is1,is2)*(1.d0-e)**2
     +				+e*(1.d0-e)*(1.d0+fa(is1,is2)*fa(is2,is1))
	    ha1=e*(1.d0-e)*(b-c)+(b-gam)*fa(is2,is1)*e**2.d0
     +					+(gam-c)*fa(is1,is2)*(1.d0-e)**2.d0
	    ha2=e*(1.d0-e)*(b-c)+(gam-c)*fa(is2,is1)*e**2.d0
     +					+(b-gam)*fa(is1,is2)*(1.d0-e)**2.d0
	    hca1=e*(1.d0-e)+fa(is1,is2)*(1.d0-e)**2.d0
	    hca2=e*(1.d0-e)+fa(is2,is1)*e**2.d0
	    hcca=e*(1.d0-e)
	    Wd1=b*e-c*(1.d0-e) +(1.d0-2.d0*e)*delta
     +		-(delta-gam)*( (1.d0-e)*fa(is1,is2)-e*fa(is2,is1) ) -ha1*w
	    wd2=b*(1.d0-e)-c*e -(1.d0-2.d0*e)*delta
     +		+(delta-gam)*( (1.d0-e)*fa(is1,is2)-e*fa(is2,is1) ) -ha2*w
	    coopd1=e*(1.d0-e)*(1.d0-w) + (1.d0-e)**2
	    coopd2=e*(1.d0-e)*(1.d0-w) + e**2
	    ccd=e*(1.d0-e)*(1.d0-w)
	      if((Pin(is1).eq.0).and.(Pin(is2).eq.1)) then
	    	ga=fa(is1,is2)*e**2.d0+fa(is2,is1)*(1.d0-e)**2
     +				+e*(1.d0-e)*(1.d0+fa(is1,is2)*fa(is2,is1))
	    	ha1=e*(1.d0-e)*(b-c)  +(gam-c)*fa(is1,is2)*e**2.d0
     +		  +(b-gam)*fa(is2,is1)*(1.d0-e)**2.d0
	    	ha2=e*(1.d0-e)*(b-c)  +(b-gam)*fa(is1,is2)*e**2.d0
     +		  +(gam-c)*fa(is2,is1)*(1.d0-e)**2.d0
	    	hca1=e*(1.d0-e)+fa(is1,is2)*e**2.d0
	    	hca2=e*(1.d0-e)+fa(is2,is1)*(1.d0-e)**2.d0
	    	hcca=e*(1.d0-e)
	    	Wd1=-c*e+b*(1.d0-e)  -(1.d0-2.d0*e)*delta
     +		 +(delta-gam)*( (1.d0-e)*fa(is2,is1)-e*fa(is1,is2) ) -ha1*w
	    	wd2=-c*(1.d0-e)+b*e +(1.d0-2.d0*e)*delta
     +		 -(delta-gam)*( (1.d0-e)*fa(is2,is1)-e*fa(is1,is2) ) -ha2*w
	    	coopd2=e*(1.d0-e)*(1.d0-w) + (1.d0-e)**2
	    	coopd1=e*(1.d0-e)*(1.d0-w) + e**2
	      endif
	  endif
	  nRC=1.d0/(1.d0-ga*w)
	  pay1=(1.d0-w)*(pay1+ha1*w/(1.d0-ga*w)+Wd1*nRC)
	  pay2=(1.d0-w)*(pay2+ha2*w/(1.d0-ga*w)+Wd2*nRC)

	  coop1=(1.d0-w)*(hca1*w/(1.d0-ga*w)+coopd1*nRC)
	  coop2=(1.d0-w)*(hca2*w/(1.d0-ga*w)+coopd2*nRC)
	  def1=(1.d0-w)*((ga-hca1)*w/(1.d0-ga*w)+(1.d0-coopd1*nRC))
	  def2=(1.d0-w)*((ga-hca2)*w/(1.d0-ga*w)+(1.d0-coopd2*nRC))
	  ccm=(1.d0-w)*(hcca*w/(1.d0-ga*w)+ccd*nRC)


	  if ((TRE.eq.01).or.(TRE.eq.11)) then
	    nRC=1.d0/(1.d0-(ga*w))
	    pay1=pay1
     +		+(1.d0-nRC*(1.d0-w))*payMATOUT(Pout(is1)+1,Pout(is2)+1) ! playing after the commitment
	    pay2=pay2
     +		+(1.d0-nRC*(1.d0-w))*payMATOUT(Pout(is2)+1,Pout(is1)+1) ! playing after the commitment
	    coop1=coop1+(1.d0-nRC*(1.d0-w))*MC(Pout(is1)+1,Pout(is2)+1) ! playing after the commitment
	    coop2=coop2+(1.d0-nRC*(1.d0-w))*MC(Pout(is2)+1,Pout(is1)+1) ! playing after the commitment
	    def1=1.d0-coop1
	    def2=1.d0-coop2
	    ccm=ccm+(1.d0-nRC*(1.d0-w))*MC(Pout(is1)+1,Pout(is2)+1) ! playing after the commitment
	  endif

	  timeC=nRC*(1.d0-w)
		!print*,is1,is2, nRC,Wd1,b-c,pay1
		!print*,is1,is2, nRC,Wd2,b-c,pay2
		!if((is1.eq.1).and.(is2.eq.5)) stop
	endif
	return
	end


	
	subroutine payOUT(MC,MCC,M,b,c,e,w)
	implicit none
	double precision MCC(32,32),MC(32,32),M(32,32),b,c,e,w
	integer i,j,k
	integer errorflag, s(2)
	double precision Q(4,4),P1(5),P2(5),f(4),g(4), 
     +		Qt(4,4),Qtinv(4,4),Bt(4),p(5,2),temp

	do i=1,32
	  do j=i,32
	    s(1)=i-1
	    s(2)=j-1
	    call s2bin(p,s,e)
	    call createQf(Q,f,p)
	    Qt=-w*Q
	    forall(k=1:4) Qt(k,k)=1.d0+Qt(k,k)
	    call FINDInv(Qt, Qtinv, 4, 4, errorflag)
	    Bt=matmul(f,Qtinv)
	    Bt=(1.d0-w)*Bt
	    g(1)=b-c
	    g(2)=-c
	    g(3)=b
	    g(4)=0.d0
	    MC(i,j)=Bt(1)+Bt(2)
	    MCC(i,j)=Bt(1)
	    M(i,j)=dot_product(g,Bt)
	    g(2)=b
	    g(3)=-c
	    MC(j,i)=Bt(1)+Bt(3)
	    MCC(j,i)=Bt(1)
	    M(j,i)=dot_product(g,Bt)
	  enddo
	enddo
	return
	end

	subroutine s2bin(p,s,e)
	integer s(2)
	double precision e, p(5,2)
	integer it,jt
	double precision tmp, tmp1
	p=0.d0+e
	do it=1,2
	  tmp1=s(it)
	  do jt=1,4
	    tmp=int(tmp1/2.d0)
	    if(tmp.ne.(tmp1/2)) p(6-jt,it)=1.d0-e
	    tmp1=tmp
	  enddo
	  if(tmp.eq.1) p(1,it)=1.d0-e
	enddo
	return
	end

      	subroutine createQf(Q,f,p)
	implicit none
	double precision p(5,2), Q(4,4), f(4)
	Q(1,1)=p(1+1,1)*p(1+1,2)
	Q(1,2)=p(1+1,1)*(1.d0-p(1+1,2))
	Q(1,3)=(1.d0-p(1+1,1))*p(1+1,2)
	Q(1,4)=(1.d0-p(1+1,1))*(1.d0-p(1+1,2))
	Q(2,1)=p(2+1,1)*p(3+1,2)
	Q(2,2)=p(2+1,1)*(1.d0-p(3+1,2))
	Q(2,3)=(1.d0-p(2+1,1))*p(3+1,2)
	Q(2,4)=(1.d0-p(2+1,1))*(1.d0-p(3+1,2))
	Q(3,1)=p(3+1,1)*p(2+1,2)
	Q(3,2)=p(3+1,1)*(1.d0-p(2+1,2))
	Q(3,3)=(1.d0-p(3+1,1))*p(2+1,2)
	Q(3,4)=(1.d0-p(3+1,1))*(1.d0-p(2+1,2))
	Q(4,1)=p(4+1,1)*p(4+1,2)
	Q(4,2)=p(4+1,1)*(1.d0-p(4+1,2))
	Q(4,3)=(1.d0-p(4+1,1))*p(4+1,2)
	Q(4,4)=(1.d0-p(4+1,1))*(1.d0-p(4+1,2))
	f(1)=p(1,1)*p(1,2)
	f(2)=p(1,1)*(1.d0-p(1,2))
	f(3)=(1.d0-p(1,1))*p(1,2)
	f(4)=(1.d0-p(1,1))*(1.d0-p(1,2))
      	return
      	end


     	SUBROUTINE FINDInv(matrix, inverse, n, nMAX, errorflag)
!Subroutine to find the inverse of a square matrix
!Author : Louisda16th a.k.a Ashwith J. Rego
!Reference : Algorithm has been well explained in:
!http://math.uww.edu/~mcfarlat/inverse.htm           
!http://www.tutor.ms.unimelb.edu.au/matrix/matrix_inverse.html
	IMPLICIT NONE
	!Declarations
	INTEGER, INTENT(IN) :: n, nMAX
	INTEGER, INTENT(OUT) :: errorflag  !Return error status. -1 for error, 0 for normal
	double precision, INTENT(IN), DIMENSION(nMAX,nMAX) :: matrix  !Input matrix
	double precision, INTENT(OUT), DIMENSION(nMAX,nMAX) :: inverse !Inverted matrix
	
	LOGICAL :: FLAG = .TRUE.
	INTEGER :: i, j, k, l
	double precision :: m
	double precision, DIMENSION(n,2*n) :: augmatrix !augmented matrix
	
	!Test for invertibility
	!Augment input matrix with an identity matrix
	DO i = 1, n
		DO j = 1, 2*n
			IF (j <= n ) THEN
				augmatrix(i,j) = matrix(i,j)
			ELSE IF ((i+n) == j) THEN
				augmatrix(i,j) = 1.d0
			Else
				augmatrix(i,j) = 0.d0
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
					PRINT*, "Matrix is non - invertible_"
					inverse = 0.d0
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
			PRINT*, "Matrix is non - invertible ."
			inverse = 0.d0
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



	include "sub_payoffM.f"

	program payoffM

	implicit none
	include "common.h"

	integer i,j,k,m, nS,N, ip,nump,ie,ib,iw,iepsi,idelta,TRE,ieAF,
     +			ii,jj, igam,   com,comMAT(nSmax,nSmax)
	double precision pay1, pay2, payMAT(nSmax,nSmax), beta, timeC,
     +  statD(nSmax), rhoMAT(nSmax,nSmax), cooMAT(nSmax,nSmax,3),
     +   ALLM(nmaxp,nSmax),headM(nmaxp,6),coop1,coop2,def1,def2,
     +   COOM(nmaxp,4), idb,ide,idw,idepsi,iddelta,ccm,ideAF,idgam,tsum,
     +			 TIMEM(nmaxp),timeMAT(nSmax,nSmax),COMM(nmaxp)
     	real, allocatable:: evect(:),bvect(:),wvect(:),
     +		 epsivect(:),deltavect(:),eAFvect(:),gamvect(:)
	character*80 outf, strf

	cooMAT=0.d0
	payMAT=-1.d0
	rhoMAT=0.d0
	print*,'Treatment -- 00, 10, 01, 11'
	read(*,*) TRE			!TRE: 00 (strict), 10 (basic), 01(antibasic), 11(relax),*
	print*,'Parameters g, eAF, b, error, w, epsi, delta '
	read(*,*) gam,eAF, b, e, w, epsi,delta
	print*,'File with strategies'
	read(*,*) strf
	print*,'Output file'
	read(*,*) outf


	open(10,file=strf,status='old')
	read(10,*)
	nS=0
	do
	  nS=nS+1
	  read(10,*,end=50) numS(nS), prop(nS), accep(nS),
     +			 Pout(nS), Pin(nS), fin(nS), ain(nS)
	enddo
 50	continue
	nS=nS-1
	close(10)

	do ii=1,nS
	  f(ii)=dabs(fin(ii)-eAF)
	  a(ii)=dabs(ain(ii)-eAF)
	enddo
	do ii=1,nS
	do jj=1,nS
	  fa(ii,jj)=f(ii)*a(jj)
	enddo
	enddo

	open(90,file=outf,status='unknown')
	write(90,'(A,6F8.2)') '#  (e,d,err,gam)= ',epsi,delta,e,gam

	payMAT=9999.d0
	do i=1,nS
	  do j=i,nS
	    !print*,numS(i),numS(j)	
      call anaPAY(pay1,pay2,i,j,coop1,coop2,def1,def2,ccm,timeC,com,TRE)
	    if(i.eq.j) then
		payMAT(i,i)=(pay1+pay2)/2.d0
	    	cooMAT(i,i,1)=(coop1+coop1)/2.d0
	    	cooMAT(i,i,2)=(def1+def2)/2.d0
	    else
	    	payMAT(i,j)=pay1
	    	payMAT(j,i)=pay2
	    	cooMAT(i,j,1)=coop1
	    	cooMAT(j,i,1)=coop2
	    	cooMAT(i,j,2)=def1
	    	cooMAT(j,i,2)=def2
	    endif
	    cooMAT(i,j,3)=ccm
	    cooMAT(j,i,3)=ccm
	    timeMAT(i,j)=timeC
	    timeMAT(j,i)=timeC
	    if(com.eq.-1) com=0
	    if(timeC.gt.com +cero) then
		print*, timeC, com
		stop 'Error timeC-com'
	    endif
	    comMAT(i,j)=com
	    comMAT(j,i)=com
	  enddo
	enddo



	do i=1,nS
	  write(90,'(10F22.16)') (PayMAT(i,j),j=1,nS)
	enddo

	close(90)

      stop
      end




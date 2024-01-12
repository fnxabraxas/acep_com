

	program mapea_sim

	implicit none
	include "param_mapea_sim.h"
	character*80 input,outX, outL, outLmulti
	integer i,j,k,l, ndist, node
	integer, parameter :: nmaxeqs=999
	double precision eps, y1(nSb), ysimu(nmaxeqs,nSb),yprob(nmaxeqs,2)


	write(*,*) 'Payoff matrix file: '
	read(*,*) input
	write(*,*) 'Output file (Xeq):'
	read(*,*) outX
	write(*,*) 'Output file (links):'
	read(*,*) outL
	write(*,*) 'Output file (all links):'
	read(*,*) outLmulti


	eps=0.01d0
	call readM(input)

			print*,'---------'
	call iniall

	open(90,file=outX,status='unknown')
	write(90,'(A1,A)') '#',' node  node_bin    Xeq'
	do i=1,nSb
	  write(90,'(2I8,$)') i, nodobooinv(i)
	  do j=1,nSb
	    write(90,'(F8.4,$)') X(i,j)
	  enddo
	  write(90,*)
	enddo

	open(91,file=outLmulti,status='unknown')
	write(91,'(A1,A)') '#',' node  node_invades    node_ends  Prob'


	i=0
	nnodos=nSb

	do
	  i=i+1
	  if(i.gt.nnodos) goto 50

	  do j=1,nSb
c				if(((i.eq.3).or.(i.eq.3))
c     +.and.((j.eq.5).or.(j.eq.5))) then
	    if(X(i,j).eq.0) then  ! invado con cada especie excepto con las ya existentes
	      print*,i,j !,nnodos

	      y1=0.d0
	      y1(j)=eps
	      do k=1,nSb
		if (X(i,k).ne.0) y1(k)=X(i,k)-(eps/nX(i))
	      enddo

	      node=0
	      do k=1,nSb
            	node=node+(ceiling(X(i,k)))*(2.**(k-1))
	      enddo

	      call simuevol(node,y1,ysimu,yprob,ndist)
c		    do l=1,ndist
c			print*,l
c			print*,(ysimu(l,k),k=1,nSb)
c			print*,'---', (yprob(l,k),k=1,2)
c		    enddo
	      call add(i,j,ysimu,yprob,ndist)

	    endif
c				endif ! para debuging
	  enddo   ! fin -- invasiones con cada especie
				
	enddo     !  fin -- todos los nodos
 50	continue

	do i=nSb+1,nnodos
	  node=0
	  do k=1,nSb
            node=node+(ceiling(X(i,k)))*(2**(k-1))
	  enddo
	  write(90,'(2I8,$)') i,node
	  do j=1,nSb
	    write(90,'(F8.4,$)') X(i,j)
	  enddo
	  write(90,*)
	enddo

	!do i=1,Nc
	!  if(nodoboo(i).ne.0) print*,i,nodoboo(i)
	!enddo

	print*,'Link list: '
	do i=1,Nc
	  if(lk(i).ne.0) print*,i,lk(i)
	enddo

	close(90)
	close(91)


	stop
	end



	subroutine add(i,j,ysimu,yprob,ndist)

	implicit none
	include "param_mapea_sim.h"
	integer i,j,ndist
	integer, parameter :: nmaxeqs=999
	double precision ysimu(nmaxeqs,nSb), yprob(nmaxeqs,2)

	double precision mediax(nSb), critsemnode
	integer id, k, node, inode,difer


			!print*,i,j,ndist
	critsemnode=0.1

	do id=1,ndist

	    node=0
	    do k=1,nSb
              node=node+(ceiling(ysimu(id,k)))*(2**(k-1))
	    enddo

	    inode=nodoboo(node)
	    if (inode.gt.0) then
		do
		  difer=0
		  do k=1,nSb
		    mediax(k)=(X(inode,k)+ysimu(id,k))/2.d0
		    !if((abs(X(inode,k)-ysimu(id,k))/mediax(k)).gt.critsemnode)
		    if((abs(X(inode,k)-ysimu(id,k))).gt.critsemnode)
     +							then
			difer=1
			exit
		    endif
		  enddo
		  if(difer.eq.0) then  ! el nodo ya existía; pone Xeq medio entre nuevo y antiguo
		    do k=1,nSb
			X(inode,k)=mediax(k)
		    enddo
		    write(91,'(3I8,2F8.4)') i,j,inode,(yprob(id,k),k=1,2)
			!print*,'+++++++'
		    exit
		  endif

		  if(lk(inode).eq.0) then  ! llega al final de lk parcial
		    nnodos=nnodos+1
		    lk(inode)=nnodos
		    lk(nnodos)=0
		    do k=1,nSb
		  	X(nnodos,k)=ysimu(id,k)
		        if(ysimu(id,k).gt.0) nX(nnodos)=nX(nnodos)+1
		    enddo
		    write(91,'(3I8,2F8.4)') i,j,nnodos,(yprob(id,k),k=1,2)
		    print*,'Xeq añadido a la LK: ',nnodos,'. Anterior: ',inode
		    print*,(X(nnodos,k),k=1,nSb)
		    print*,(X(inode,k),k=1,nSb)
		    exit
		  else
		    inode=lk(inode)
		  endif
		enddo


	    else
		nnodos=nnodos+1
		do k=1,nSb
		  X(nnodos,k)=ysimu(id,k)
		  if(ysimu(id,k).gt.0) nX(nnodos)=nX(nnodos)+1
		enddo
		nodoboo(node)=nnodos
		lk(nnodos)=0
		write(91,'(3I8,2F8.4)') i,j,nnodos,(yprob(id,k),k=1,2)
			!print*,'kkkkk',i,j,nnodos,(yprob(id,k),k=1,2)
	    endif
			!print*,'*** ',node,inode,(yprob(id,1))
	enddo


	return
	end





	subroutine readM(inp)

	implicit none
	character*80 inp
	integer i,j
	include "param_mapea_sim.h"
	open(10,file=inp,status='old')
	read(10,*)
	do i=1,nSb
	    read(10,*) (M(i,j), j=1,nSb)
	enddo
	close(10)

	return
	end


	subroutine iniall

	implicit none
	integer i
	include "param_mapea_sim.h"
	X=0.
	nodoboo=0
	nodobooinv=0
	nX=0
	lk=0
	do i=1,nSb
	    X(i,i)=1.
	    nX(i)=1
	    nodoboo(2**(i-1))=i
	    nodobooinv(i)=2**(i-1)
	enddo	

	return
	end



	subroutine simuevol(nodeini,yini,ysimu,yprob,ndist)

	implicit none
	include "param_mapea_sim.h"
	double precision yini(nSb), ysimu(999,nSb),yprob(999,2)
	integer ndist,nodeini

	integer nsim,isim,nindiv,seed,np,k,k2,yp(nSb),ntp,ip,jp,ic,ip2,
     +			p1,p2,npi,ibus,ypi(nSb),ypant(nSb), yt
	integer iit, yindex(nSb),yindexi(nSb), difer  ,nmaxit,
     +		 noderes, cibus   !!!, startconv, nitsum,itsum,	
	real*8 rand
	double precision A(nSb,nSb),Ai(nSb,nSb), numran, difpaymax,
     +   		     yac, difpaymaxi, pp,  calcdpm, 
     +		 	 Pri,Prij, payi, payj, paymed,denoPri,numPri,Pac,
     +				probfin, probcamb, sumyp(nSb), sumypant(nSb),
     +				 critsemix, critsemyp, mediayp(nSb), probcut, 
     +					TinterM,Tacum,Tact,Tstart,TacumT
     +						

	seed=123456
	!!!nitsum=100000 ! intervalo para hacer la media para convergencia
	TinterM=1.d6
	critsemix=0.01d0 ! ! en diferencia, no en porcentaje
	nsim=1000
	nindiv=1000
	!!!startconv=100000 ! iteración en la que se empieza a buscar convergencia	
	Tstart=1.d6
	probcut=0 !0.1 ! corte en probabilidad (por debajo se ignoran nodos)
	critsemyp=0.001d0*nindiv
	  !nmaxit=20*nindiv
	!probfin=1e-3
	ysimu=0.d0
	yprob=0.d0
	!!!!!call srand(seed)
			!print*,yini
	call init_random_seed()

	np=0
	ntp=0
	yindex=0
	yp=0
	do k=1,nSb
	  if(yini(k).gt.0.d0) then
	    np=np+1
	    yp(np)=nint(yini(k)*nindiv)
	    yindex(np)=k
	    ntp=ntp+yp(np)
	  endif
	enddo

	if(ntp.ne.nindiv) then	! a veces sale 1 de más o de menos por el redondeo
	    if(ntp.ge.(nindiv-2)) then
		 yp(p1)=yp(p1)+nindiv-ntp 
	    else
		print*, (yp(k),k=1,np)
		stop 
	    endif
	endif


	A=0.d0
	ip=0
	do k=1,nSb
	 if(yini(k).gt.0.d0) then
	   ip=ip+1
	   jp=0
	   do k2=1,nSb
             if(yini(k2).gt.0.) then
		jp=jp+1
	    	A(ip,jp)=M(k,k2)
	     endif
	   enddo
	 endif
	enddo
	difpaymax=calcdpm(A,np)
		  !print*,yp(1),yp(2)
		  !print*,A(1,1),A(1,2)
		  !print*,A(2,1),A(2,2)
		  !print*,difpaymax
	Ai=A
	ypi=yp
	npi=np
	yindexi=yindex
	difpaymaxi=difpaymax
					
	do isim=1,nsim

	 A=Ai
	 yp=ypi
	 np=npi
	 yindex=yindexi
	 difpaymax=difpaymaxi
				
	 ic=0
	 !iit=0  ! lleva cuenta de número de interaciones sin que nada cambie
	 !!!itsum=0
	 Tacum=0.d0
	 TacumT=0.d0
	 Tact=0.d0
	 Tact=0.d0
	 do k=1,nSb
	   sumyp(k)=0.d0
	   sumypant(k)=0.d0
	 enddo
	 do

	   ibus=0
	   do k=1,nSb 
	    ibus=ibus+yp(k)
	   enddo
	   if(ibus.ne.nindiv) then	! a veces sale 1 de más o de menos por el redondeo
		
	    if(ibus.ge.(nindiv-2)) then
		 yp(1)=yp(1)+nindiv-ibus 
	    else
		print*, (yp(k),k=1,np)
		stop 
	    endif
	   endif


	   ic=ic+1
				
	   denoPri=0.d0
	   do k=1,np
	     denoPri=denoPri+(yp(k)**2.d0)
	   enddo
	   denoPri=nindiv**2.d0-denoPri
	   paymed=0.
	   do k=1,np
	     do k2=1,np
	       paymed=paymed+A(k,k2)*yp(k)*yp(k2)
	     enddo
	   enddo
	   paymed=paymed/nindiv/nindiv
	   !!!if(difpaymax.lt.1.d-14) difpaymax=1.   ! para evitar inderterminación
			!print*,'----'

	   probcamb=denoPri/2/nindiv/(nindiv-1)
	   !if(probcamb.lt.probfin) then  
	   !	print*,'Probabilidad de cambio muy pequeña '
	   ! 	print*,ic,probcamb
	   ! 	stop
	   !endif
			!print*, ic,yp(1),yp(2),probcamb
		!print*, 0.5-((denoPri-nindiv)/2/nindiv/(nindiv-1))
 113	   continue

	   call random_number(numran)  !!!!! rand(0)
	   Pac=0.d0
	   p1=0				
	   do
	     p1=p1+1
	     if(p1.gt.np) then
			print*,'WARNING:  p1>np: ',p1,np
			print*, paymed,payi,p1,numran,Pri,Pac
			print*, nindiv,yp(p1-1),nindiv*(paymed-payi)/difpaymax
			print*, difpaymax
			print*,numPri,denoPri
			stop
			goto 113
		stop
	     endif
	     payi=0.
	     do k=1,np
		payi=payi+A(p1,k)*yp(k)
	     enddo
	     payi=payi/nindiv
	     if((difpaymax.lt.1.d-14).or.(abs(paymed-payi).lt.1.d-14))
     +								then
		numPri=nindiv-yp(p1)
	     else
		numPri=nindiv-yp(p1)+nindiv*(paymed-payi)/difpaymax
	     endif
	     Pri=yp(p1)*numPri/denoPri
	     Pac=Pac+Pri
			!print*, nint(numran*1.e5),nint(Pac*1.e5)
			!print*, paymed,payi,p1,numran,Pri,Pac
			!stop
	     !if(abs(numPri).lt.1e-10) goto 103
	     !if(nint((nindiv-1)*numran).le.nint((nindiv-1)*Pac)) goto 101
		if(nint(numran*1.e5).le.nint(Pac*1.e5)) goto 101
	   enddo
 101	   continue
	   call random_number(numran)  !!!!! rand(0)
	   Pac=0.d0
	   p2=0				
	   do
	     p2=p2+1
	     if(p2.gt.np) then
		print*,'p2>np: ',p2,np,npi
			print*,(yp(k),k=1,npi)
			print*, paymed,payi,payj,p1,p2,numran,Prij,Pri,Pac
			print*, difpaymax,numPri, isim,ic
			goto 113
		stop
	     endif
	     if(p2.eq.p1) p2=p2+1
	     payj=0.d0
	     do k=1,np
		payj=payj+A(p2,k)*yp(k)
	     enddo
	     payj=payj/nindiv
	      if(abs(payj-payi).gt.(difpaymax+1.d6)) then
		print*,payj,payi,payj-payi,difpaymax
		print*
		print*, yp(p1), yp(p2)
		print*,A(p1,p1),A(p1,p2)
		print*,A(p2,p1),A(p2,p2)
		stop 'Error: difpaymax'
	      endif

	     if((difpaymax.lt.1.d-14).or.(abs(payj-payi).lt.1.d-14))
     +								then
		Prij=yp(p2)/numPri
	     else
		Prij=yp(p2)*(1.d0+(payj-payi)/difpaymax)/numPri
	     endif
	     Pac=Pac+Prij
		!print*, nint(numran*1.e5),nint(Pac*1.e5)
			
	     !if(nint(nindiv*0.05*numran).le.nint(nindiv*0.05*Pac)) goto 102
	         if(nint(numran*1.e5).le.nint(Pac*1.e5)) goto 102
	   enddo
 102	   continue
			
	   yp(p1)=yp(p1)-1
	   yp(p2)=yp(p2)+1
		
	   !!! if(ic.gt.startconv) then
	   Tact=1.d0/(denoPri/(2.d0*nindiv*(nindiv-1)) )  !!! CUIDADO
	   TacumT=TacumT+Tact
		
	   if(TacumT.gt.Tstart) then

	     Tacum=Tacum+Tact
	     !!!itsum=itsum+1
	     do k=1,np
		!!!sumyp(k)=sumyp(k)+1.*yp(k)/nitsum
		sumyp(k)=sumyp(k)+1.*yp(k)*Tact
	     enddo
	     !!!if(itsum.eq.nitsum) then
	     if(Tacum.ge.TinterM) then
		sumyp=nint(sumyp/Tacum)
			
		difer=0
		do k=1,nSb
		  mediayp(k)=(sumyp(k)+sumypant(k))/2.d0
		  !if((abs(sumyp(k)-sumypant(k))/mediayp(k)).gt.critsemyp) then
		  if((abs(sumyp(k)-sumypant(k))).gt.critsemyp) then
		    difer=1
		    exit
		  endif
		  !stop
		enddo

 		if(difer.eq.0) then     ! ha encontrado convergencia
		  ibus=0
		  do k=1,nSb
		    yp(k)=nint(mediayp(k))  
		    ibus=ibus+yp(k)
		  enddo
		  if(ibus.ne.nindiv) then	! a veces sale 1 de más o de menos por el redondeo
		    if(ibus.ge.(nindiv-2)) then
			 yp(p1)=yp(p1)+nindiv-ibus 
		    else
			print*, (yp(k),k=1,np)
			stop 
		    endif
		  endif
		  goto 103  
		endif
		!!!itsum=0
		Tacum=0.
		sumypant=sumyp
		sumyp=0.
	     endif

	   endif

				!print*,p1,p2,yp(p1),yp(p2)
c	   numran=drand(0)
c	   yac=0.
c	   p1=0
c	   do
c	     p1=p1+1
c	     yac=yac+yp(p1)
c				!if(isim.eq.34) print*,numran,yac,p1
c	     if(numran*nindiv.lt.yac) goto 101
c	   enddo		   
c  101      continue
c		!if(p1.eq.1) p2=2
c		!if(p1.eq.2) p2=1
c			!print*,p1, p2
c	   !yp(p1)=yp(p1)-1  ! tomo p1, luego irá al grupo p1 o p2
c	   numran=drand(0)
c	   yac=0
c	   p2=0
c	   do
c	     p2=p2+1
c	     yt=yp(p2)
c	     if(p2.eq.p1) yt=yt-1   ! no tengo en cuenta a p1
c	     yac=yac+yt
c	     if(numran*(nindiv-1).lt.yac) goto 102
c	   enddo		   
c  102      continue
c
c
c	   if(p1.eq.p2) then   		! p1=p1 => p1 vuelve a su grupo
c	       !yp(p1)=yp(p1)+1
c	       iit=iit+1
c	   else				! p1 y p2 son diferentes
c		
c	    if(difpaymax.eq.0) then
c	     pp=0.5
c	    else
c	     pp=0
c	     do k=1,np
c	       pp=pp+ A(p2,k)*yp(k) -A(p1,k)*yp(k)
c	     enddo
c	     pp=pp/nindiv/(2*difpaymax) +0.5
c	    endif
c
c	    numran=drand(0)
c			   !if(isim.eq.34) then
c			   !	print*, (yp(ip),ip=1,np) 
c			   !	print*, yindex(p1),yindex(p2),p1,p2
c			   !endif
c	    
c	    if(numran.lt.pp) then  	! p1 va a p2
c	       yp(p2)=yp(p2)+1
c	       yp(p1)=yp(p1)-1
c	       iit=0
	       if(yp(p1).eq.0) then     ! especie de p1 ha desaparecido

		ypant=yp
		ip=0
		do k=1,np
	  	  if(yp(k).gt.0.) then
	    	    ip=ip+1
		    yp(ip)=ypant(k)
		    yindex(ip)=yindex(k)
	    	    jp=0
	    	    do k2=1,np
              	      if(ypant(k2).gt.0.) then
		   	jp=jp+1
	    		A(ip,jp)=A(k,k2)
	      	      endif
	    	    enddo
	  	  endif
		enddo
		yp(np)=0
	        np=np-1

		ibus=0
		do ip=1,np
		  ibus=ibus+yp(ip)
		enddo
		if(ibus.lt.nindiv) print*,'suma menor que nind: ',isim,ic,ibus
	        difpaymax=calcdpm(A,np)

	       ic=0
	       sumypant=0.d0
	       !!!itsum=0
	       Tacum=0.d0
	       endif	
c	    else			      ! p1 vuelve a su grupo
c	       !yp(p1)=yp(p1)+1
c	       iit=iit+1
c	    endif
c	                    !print*, p1,p2,(yp(ip),ip=1,np), iit
c	   endif

	   if((np.eq.1)) goto 103   !.or.(iit.gt.nmaxit)) goto 103


	 enddo  ! hasta encontrar convergencia 
  103    continue



	 ibus=0
	 do k=nSb,1,-1
	   if(yp(k).ne.0) then
	        ibus=ibus+yp(k)
	 	yp(yindex(k))=yp(k)
	 	if(yindex(k).ne.k) yp(k)=0
	   endif
	 enddo
	 if(ibus.ne.nindiv) then
	    	print*, (yp(k),k=1,nSb)
	    	print*, isim,ic
	    	stop 
	 endif


	 if(isim.eq.1) then
	  yprob=0.d0
	  do k=1,nSb
	    ysimu(1,k)=yp(k)
	  enddo
	  yprob(1,1)=1.d0
	  ndist=1
         else

	  ibus=1
  104     continue
	  do k=1,nSb
	    if(yp(k).ne.ysimu(ibus,k)) then
	      ibus=ibus+1
	      if(ibus.le.ndist) goto 104
	      if(ibus.gt.ndist) then
		ndist=ndist+1
	        do k2=1,nSb
		  ysimu(ndist,k2)=yp(k2)
		enddo
		yprob(ndist,1)=1.d0
		goto 105	
	      endif
	    endif
	  enddo
	
	  do k=1,nSb
	    ysimu(ibus,k)=yp(k)
	  enddo
	  yprob(ibus,1)=yprob(ibus,1)+1.d0
  105	  continue

	 endif


	enddo   ! fin todas simulaciones

	if(ndist.gt.999) then
	  print*,'ndist demasiado grande.'
	  stop
	endif	

	do ip=1,ndist
	  do k=1,nSb
	    ysimu(ip,k)=ysimu(ip,k)/nindiv
	  enddo
	  yprob(ip,2)=((yprob(ip,1)*(nsim-yprob(ip,1))/nsim)**0.5d0)/nsim
	  yprob(ip,1)=yprob(ip,1)/nsim
	enddo


	do ip=1,ndist
	  do ip2=ip+1,ndist
 106        continue
	    if(ip2.gt.ndist) exit

            difer=0
	    do k=1,nSb
c	       if((2*abs(ysimu(ip,k)-ysimu(ip2,k))/(ysimu(ip,k)+ysimu(ip2,k)))
c     +               .gt.critsemix) difer=1
	       if(((abs(ysimu(ip,k)-ysimu(ip2,k))).gt.critsemix).or.
     +                (ysimu(ip,k).eq.0).and.(ysimu(ip2,k).ne.0).or. 
     +		      (ysimu(ip,k).ne.0).and.(ysimu(ip2,k).eq.0)  ) difer=1
	    enddo
	    if(difer.eq.0) then
		do k=1,nSb
		  ysimu(ip,k)=(ysimu(ip,k)*yprob(ip,1)+
     +             ysimu(ip2,k)*yprob(ip2,1))/(yprob(ip,1)+yprob(ip2,1))
		  if(ip2.lt.ndist) ysimu(ip2,k)=ysimu(ndist,k)		  
	 	enddo
		yprob(ip,2)=(yprob(ip,2)**2+yprob(ip2,2)**2.d0-
     +              2.d0*yprob(ip,1)*yprob(ip2,1)/nsim )
		if(yprob(ip,2).lt.0) yprob(ip,2)=0
		yprob(ip,2)=yprob(ip,2)**0.5d0
 		yprob(ip,1)=yprob(ip,1)+yprob(ip2,1)
		if(ip2.lt.ndist) yprob(ip2,1)=yprob(ndist,1)
		if(ip2.lt.ndist) yprob(ip2,2)=yprob(ndist,2)
		ndist=ndist-1
	        if(ip2.le.ndist) goto 106
	    endif

	  enddo
	enddo

	do ip=ndist,1,-1
	  noderes=0
	  do k=1,nSb
	     noderes=noderes+(ceiling(ysimu(ip,k)))*(2**(k-1))
	  enddo
	  if((yprob(ip,1).le.probcut).and.(noderes.ne.nodeini)) then
	     print*,'Ingnorando nodo con P= ',yprob(ip,1),yprob(ip,2)
	     if(ip.ne.ndist) then
		yprob(ip,1)=yprob(ndist,1)
		yprob(ip,2)=yprob(ndist,2)
		do k=1,nSb
		  ysimu(ip,k)=ysimu(ndist,k)
	    	enddo
	     endif
	     ndist=ndist-1
	  endif
	enddo

		!**! print*,'ndist: ',ndist

	return
	end




	double precision function calcdpm(A,np)

	implicit none
	include "param_mapea_sim.h"
	double precision A(nSb,nSb)
	integer np
	integer k,k2, kb,k2b
	!double precision maximo, minimo
	calcdpm=0.
	do k=1,np
	  do k2=1,np
		do kb=k+1,np
		  !do k2b=1,np
	  		if(abs(A(k,k2)-A(kb,k2)).gt.calcdpm) 
     +				calcdpm=abs(A(k,k2)-A(kb,k2)) !+1.d-8
		  !enddo
		enddo
	  enddo
	enddo
	!calcdpm=maxval(A)-minval(A)

	return
	end


          SUBROUTINE init_random_seed()
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed
          
            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))
          
            CALL SYSTEM_CLOCK(COUNT=clock)
          
            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
				!seed=123456
            CALL RANDOM_SEED(PUT = seed)
          
            DEALLOCATE(seed)
          END SUBROUTINE




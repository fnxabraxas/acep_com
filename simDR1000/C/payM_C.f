

      program PayoffM
	implicit none
	integer, parameter :: Ns=10
	character*80 output, inpf
	double precision eps ,Rp,Sp,Tp,Pp,PayM(Ns,Ns)
	double precision e,d
	integer i,j,k, Sc(99), Si(99), So(99), comi

	Rp=1.d0
	Pp=0.d0
	Sp=-1.d0
	Tp=2.d0

	write(*,*) 'Strategies file: '
	read(*,*) inpf
	write(*,*) 'Epsilon: '
	read(*,*) eps
	write(*,*) 'epsilon, delta (R=1,P=0,T=2,S=-1 considered): '
	read(*,*) e,d 
	write(*,*) 'Output file (payoff matrix): '
	read(*,*) output
		 
	open(10,file=inpf,status='old')
	read(10,*)
	do i=1,Ns
	  read(10,*) Sc(i),Si(i),So(i)
	enddo
	close(10)

	open(90,file=output,status='unknown')
	write(90,'(A,6F8.2)') '#  (e,d,R,S,T,P)= ',e,d,Rp,Sp,Tp,Pp
	
	do i=1,Ns
	  do j=i,Ns

	    if( ((Sc(i).ne.1).and.(Sc(j).ne.1)) .or. 
     &			((Sc(i).eq.0).or.(Sc(j).eq.0)) ) then
		comi=0
	      	payM(j,i)=0.d0
	      	payM(i,j)=0.d0
	    else
		comi=1
	     if (Sc(i).eq.1) then
	      if (Sc(j).eq.1) then
		payM(i,j)=-e/2.d0
		payM(j,i)=-e/2.d0
	      else
		payM(i,j)=-e
		payM(j,i)=0.d0
	      endif
	    elseif (Sc(j).eq.1) then
		payM(j,i)=-e
		payM(i,j)=0.d0
	    else
	      	payM(j,i)=0.d0
	      	payM(i,j)=0.d0		
	     endif
	    endif

	    if(comi.eq.0) then
	      if(So(i).eq.1) then
		if(So(j).eq.1) then
		  payM(i,j)=payM(i,j)+Rp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Rp		  
		else
		  payM(i,j)=payM(i,j)+Sp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Tp
		endif
	      else
		if(So(j).eq.1) then
		  payM(i,j)=payM(i,j)+Tp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Sp
		else
		  payM(i,j)=payM(i,j)+Pp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Pp
		endif
	      endif

	    else
	      if(Si(i).eq.1) then
		if(Si(j).eq.1) then
		  payM(i,j)=payM(i,j)+Rp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Rp		  
		else
		  payM(i,j)=payM(i,j)+Sp+d
		  if(i.ne.j) payM(j,i)=payM(j,i)+Tp-d
		endif
	      else
		if(Si(j).eq.1) then
		  payM(i,j)=payM(i,j)+Tp-d
		  if(i.ne.j) payM(j,i)=payM(j,i)+Sp+d
		else
		  payM(i,j)=payM(i,j)+Pp
		  if(i.ne.j) payM(j,i)=payM(j,i)+Pp
		endif
	      endif

	    endif

	  enddo
	enddo


	do i=1,nS
	  write(90,'(10F22.16)') (PayM(i,j),j=1,Ns)
	enddo

	close(90)

      stop
      end




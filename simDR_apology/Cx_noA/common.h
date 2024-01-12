
	integer, parameter :: NSmax=99, nmaxp=999999
	double precision, parameter :: c=1.d0, cero=1.d-30

	integer numS(nSmax), prop(nSmax),accep(nSmax), Pout(nSmax)
	double precision e,b,w,epsi,delta,Pin(nSmax),fin(nSmax),
     +		  f(nSmax),eAF,gam,a(nSmax),ain(nSmax),fa(nSmax,nSmax)

	common /param/	e,b,w,epsi,delta,eAF,gam
	common /strategies/ numS, prop,accep,Pout,Pin,fa
